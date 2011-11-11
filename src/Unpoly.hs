------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Unpoly (
  unpolyProgram
  ) where

import Choco
import Const
import Id
import Outputable
import Panic
import SrcLoc
import TcSyn
import Types
import Var

import Control.Monad.State
import qualified Data.IntSet as S
import qualified Data.Map as M

{- Function name mangling -}
-- int   : 'i'
-- bool  : 'i' (the representation of boolean value is int)
-- float : 'f'
-- unit  : 'u'
-- Array : 'A' + element 
-- Tuple : 'T' + elements + 'P'
-- Function : 'F' + argments + 'N' + result

mangle :: String -> Type -> String
mangle name ty = name ++ "_" ++ suffix ty
  where
  suffix UnitT  = "u"
  suffix BoolT  = "i"
  suffix IntT   = "i"
  suffix FloatT = "f"
  suffix (FunT args ret) 
    = "F" ++ concatMap suffix args ++ "N" ++ suffix ret
  suffix (ArrayT t) = "A" ++ suffix t
  suffix (TupleT elems) = "T" ++ concatMap suffix elems ++ "P"
  suffix t = panic $ "unresolved polymorphic type: " ++ show t

{- Expansion Monad -}
type P a = StateT Env ChocoM a

data Env = Env {
  poly_map   :: M.Map Var (VarInfo, [(Type, Var)]),
  rn_map     :: M.Map Var Var,
  ty_map     :: M.Map TyId Type,
  need_retry :: Bool,
  retry_count :: Int
  }
  deriving (Show)

data VarInfo = Unused | Ready | NotReady
  deriving (Show)

initEnv = Env {
  poly_map   = M.empty,
  rn_map     = M.empty,
  ty_map     = M.empty,
  need_retry = False,
  retry_count = 0
  }

{- Utilities -}
isPoly :: TyScheme -> Bool
isPoly (TyScheme ids _) = not $ S.null ids

fullSpecified :: TyScheme -> Bool
fullSpecified (TyScheme ids ty) = check ty
  where
  check (TyVar _)       = False
  check (FunT args ret) = all check args && check ret
  check (ArrayT t)      = check t
  check (TupleT ts)     = all check ts
  check _               = True

specifyType :: Type -> P Type
specifyType t = case t of
  TyVar id -> 
    do m <- gets ty_map
       return $ M.findWithDefault t id m
  FunT args ret ->
    do args' <- mapM specifyType args
       ret'  <- specifyType ret
       return $ FunT args' ret'
  ArrayT e -> return . ArrayT =<< specifyType e
  TupleT es -> return . TupleT =<< mapM specifyType es
  t -> return t
    
unpoly :: [LTcStmt] -> P [LTcStmt]
unpoly [] = return []
unpoly (L loc (TcEvalS e) : cont)
  = do e' <- unpolyExpr e
       cont' <- unpoly cont
       return (L loc (TcEvalS e') : cont')
unpoly (L loc (TcValueS bind@(pat, e)) : cont)
  = do scanPat pat
       cont' <- unpoly cont  -- unpoly continuations first
       binds <- unpolyBind bind
       return $ map (\b -> L loc (TcValueS b)) binds ++ cont'

unpolyExpr exp = do
  t <- specifyType (expr_type exp)
  let e = exp{ expr_type = t }
  case expr_desc e of
    TcVarE v -> do
      env@Env{ poly_map = m, rn_map = r } <- get
      case M.lookup v m of
        Just (unused, entries) -- this is polymorphic variable
          | fullSpecified (var_type v)
          -> let TyScheme _ ty = var_type v in
             case lookup ty entries of
              Just v' -> return e{ expr_desc = TcVarE v' }
              Nothing -> do
                id' <- lift$newVarId (isGlobal v)
                let name' = mangle (var_name v) ty
                let v' = mkVar name' id' (toScheme ty)
                put env{ poly_map = 
                  M.insert v (Ready, (ty, v') : entries) m }
                return e{ expr_desc = TcVarE v' }
          | otherwise
          -> do let TyScheme _ ty = var_type v 
                scheme <- closure =<< specifyType ty
                put env{ poly_map = M.insert v (NotReady, entries) m }
                return e{ expr_desc = TcVarE v{ var_type = scheme } }

        Nothing -- this is not polymorphic variable
          -> case M.lookup v r of
              Just v' -> return e{ expr_desc = TcVarE v' }
              Nothing -> return e
    TcLitE l -> return e
    TcAppE f args -> do
      f'    <- unpolyExpr f
      args' <- mapM unpolyExpr args
      return e{ expr_desc = TcAppE f' args' }
    TcPrefixE op e1 -> do
      e1' <- unpolyExpr e1
      return e{ expr_desc = TcPrefixE op e1' }
    TcInfixE op e1 e2 -> do
      e1' <- unpolyExpr e1
      e2' <- unpolyExpr e2
      return e{ expr_desc = TcInfixE op e1' e2' }
    TcLetE flag bind@(pat, _) cont -> do
      scanPat pat
      cont'  <- unpolyExpr cont
      binds <- unpolyBind bind
      return $ foldr 
              (\b c -> e{ expr_desc = TcLetE flag b c })
              cont' binds
    TcFunE args body -> do
      args' <- mapM unpolyPat args
      body' <- unpolyExpr body
      return e{ expr_desc = TcFunE args' body' }
    {-
    TcFunE args body -> do
      env@Env{ ty_map = m } <- get
      let m' = m `M.union` polyBind 
                (FunT (map pat_type args) (expr_type body)) 
                (expr_type e)
      put env{ ty_map = m' }
      args' <- mapM unpolyPat args
      body' <- unpolyExpr body
      put env{ ty_map = m }
      return e{ expr_desc = TcFunE args' body' }
    -}
    TcCondE e1 e2 e3 -> do
      e1' <- unpolyExpr e1
      e2' <- unpolyExpr e2
      e3' <- unpolyExpr e3
      return e{ expr_desc = TcCondE e1' e2' e3' }
    TcTupleE elems -> do
      elems' <- mapM unpolyExpr elems
      return e{ expr_desc = TcTupleE elems' }
    TcSeqE e1 e2 -> do
      e1' <- unpolyExpr e1
      e2' <- unpolyExpr e2
      return e{ expr_desc = TcSeqE e1' e2' }

unpolyPat p = do
  p' <- case pat_desc p of
    TcAnyP -> return p{ pat_desc = TcAnyP }
    TcVarP v -> do
      v' <- renameVar v
      modify $ \env -> env{ rn_map = M.insert v v' (rn_map env) }
      return p{ pat_desc = TcVarP v' }
    TcTupleP pats -> do
      pats' <- mapM unpolyPat pats
      return p{ pat_desc = TcTupleP pats' }
  t <- specifyType (pat_type p')
  return p'{ pat_type = t }

unpolyBind (p@TcPat{ pat_desc = TcVarP v }, e)
  | isPoly (var_type v)
  = do env@Env{ poly_map = m, rn_map = r, ty_map = t } <- get
       let (info, instances) = m M.! v
       let TyScheme _ var_t = var_type v
       binds <- foldM (\bs (ty, v') -> do
         put env{ 
           rn_map = M.insert v v' r,
           ty_map = polyBind var_t ty `M.union` t
           }
         e' <- unpolyExpr e
         return $ (p{ pat_desc = TcVarP v' }, e') : bs
         ) [] instances
       case info of
          Unused -> return []
          Ready  -> return binds
          NotReady -> do
            modify $ \e -> e{ need_retry = True }
            return $ binds
unpolyBind (p, e)
  = do e' <- unpolyExpr e
       p' <- unpolyPat p
       return [(p', e')]

polyBind (TyVar i) t = M.singleton i t
polyBind (FunT args1 ret1) (FunT args2 ret2)
  = M.unions $ polyBind ret1 ret2 : zipWith polyBind args1 args2
polyBind (ArrayT t1) (ArrayT t2)
  = polyBind t1 t2
polyBind (TupleT t1) (TupleT t2)
  = M.unions $ zipWith polyBind t1 t2
polyBind _ _ = M.empty

scanPat TcPat{ pat_desc = TcVarP v }
  | isPoly (var_type v)
  = do env@Env{ poly_map = m } <- get
       when (M.notMember v m) $
          put env{ poly_map = M.insert v (Unused, []) m }
  | otherwise
  = return ()
scanPat _ = return ()

renameVar v = do
  let TyScheme _ var_t = var_type v
  var_t' <- specifyType var_t
  id' <- lift $ newVarId (isGlobal v)
  let v' = mkVar (var_name v) id' (toScheme var_t')
  return v'

closure :: Type -> P TyScheme
closure t = do 
  m <- gets ty_map
  let fv = freeVar t S.\\ S.fromList (M.keys m)
  return $ TyScheme fv t

{- Generate array related functions -}

{- Interface -}
unpolyProgram :: [LTcStmt] -> ChocoM [LTcStmt]
unpolyProgram stmts = evalStateT loop initEnv
  where
  loop = do stmts' <- unpoly stmts
            retry <- gets need_retry
            count <- gets retry_count
            if retry && count < 10
              then do modify $ \e ->
                        e{ need_retry = False,
                           retry_count = count + 1
                        }
                      loop
              else do env <- get
                      lift$runIO$print env
                      return stmts'
