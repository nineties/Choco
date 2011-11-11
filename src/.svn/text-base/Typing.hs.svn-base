{-# OPTION -cpp #-}
------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Typing ( 
  typeCheck,
  dumpVarMap
  ) where

import Choco
import Const
import Id
import McSyn
import Outputable
import Panic
import Primitive
import SrcLoc
import TcSyn
import Types
import Var

import Control.Monad.Error
import Control.Monad.State
import qualified Data.IntMap as I
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.IntSet as S

{- Type inference Monad -}
type TcM a = StateT TcEnv ChocoM a

data TcEnv = TcEnv {
  var_map        :: IdMap TyScheme,
  tyvar_counter  :: Id
  }

initTcEnv = TcEnv {
  var_map = I.empty,
  tyvar_counter = 0
  }

instance Show TcEnv where
  show TcEnv{ var_map = m } = show m

newTyId :: TcM TyId
newTyId = do
  e@TcEnv{ tyvar_counter = c } <- get
  put e{ tyvar_counter = c+1 }
  return c

getTyId :: TcM TyId
getTyId = do
  TcEnv{ tyvar_counter = c } <- get
  return c

{- operations on type-scheme -}
renameTySc :: TyScheme -> TcM TyScheme
renameTySc (TyScheme ids ty)
  = do newids <- replicateM (S.size ids) newTyId
       let mapping = I.fromList (zip (S.toList ids) newids)
       return $ TyScheme (S.fromList newids) (rename mapping ty)
  where
  rename mapping (TyVar v) 
    = case I.lookup v mapping of
        Just v' -> TyVar v'
        Nothing -> TyVar v
  rename m(FunT args ret) = FunT (map (rename m) args) (rename m ret)
  rename m (TupleT elems) = TupleT (map (rename m) elems)
  rename m (ArrayT t)     = ArrayT (rename m t)
  rename _ t              = t

closure :: Type -> TcM TyScheme
closure t = do TcEnv{ var_map = map } <- get
               let fv = freeVar t S.\\ S.fromList (I.keys map)
               return $ TyScheme fv t

{- Type inference -}
-- statements
inferStmts :: VarMap -> [LMcStmt] -> TcM (VarMap, [LTcStmt])
inferStmts vmap [] = return (vmap, [])
inferStmts vmap (s:ss) = do
  (vmap', s')   <- inferStmt vmap s
  (vmap'', ss') <- inferStmts vmap' ss
  return (vmap'', s':ss')

inferStmt :: VarMap -> LMcStmt -> TcM (VarMap, LTcStmt)
inferStmt vmap (L loc (McEvalS e))
  = do e' <- inferExpr vmap e
       return (vmap, L loc (TcEvalS e'))
inferStmt vmap (L loc (McValueS bind))
  = do (vmap', bind') <- inferBind vmap bind True
       return (vmap', L loc (TcValueS bind'))

-- expressions
inferExpr :: VarMap -> LMcExpr -> TcM TcExpr
inferExpr vmap (L loc (McVarE name))
  = case M.lookup name vmap of 
      Just (scheme, id, is_global) ->
        do s@(TyScheme _ ty) <- renameTySc scheme
           return $ TcExpr (TcVarE (mkVar (unName name) id s is_global)) loc ty 
      Nothing -> lift $ 
        compileError (text "undefined variable:" <+> ppr name) loc
       
inferExpr vmap (L loc (McLitE c))
  = return $ TcExpr (TcLitE c) loc (constType c)

inferExpr vmap (L loc (McAppE f args))
  = do ret_t <- return . TyVar =<<  newTyId
       f' <- inferExpr vmap f
       args' <- mapM (inferExpr vmap) args
       unify (expr_type f') (FunT (map expr_type args') ret_t) loc
       derefExpr $ TcExpr (TcAppE f' args') loc ret_t

inferExpr vmap (L loc (McInfixE op e1 e2))
  = case binopType op of
      Just t  -> do e1' <- inferExpr vmap e1
                    e2' <- inferExpr vmap e2
                    unify t (expr_type e1') loc
                    unify t (expr_type e2') loc
                    derefExpr $ TcExpr (TcInfixE op e1' e2') loc t
      Nothing -> do e1' <- inferExpr vmap e1
                    e2' <- inferExpr vmap e2
                    unify (expr_type e1') (expr_type e2') loc
                    derefExpr $ TcExpr (TcInfixE op e1' e2') loc BoolT

inferExpr vmap (L loc (McPrefixE op e))
  = do let t = preopType op
       e' <- inferExpr vmap e
       unify t (expr_type e') loc
       derefExpr $ TcExpr (TcPrefixE op e') loc t

inferExpr vmap (L loc (McFunE args body))
  = do (vmap', args', args_t) <- walk vmap args [] []
       body' <- inferExpr vmap' body
       derefExpr $ TcExpr (TcFunE args' body') loc (FunT args_t (expr_type body'))
  where
  walk m [] ps ts = return (m, reverse ps, reverse ts)
  walk m (p:ps) ps' ts
    = do (m', p', t) <- parsePat m p False
         walk m' ps (p':ps') (t:ts)

inferExpr vmap (L loc (McCondE cond e1 e2))
  = do cond' <- inferExpr vmap cond
       unify BoolT (expr_type cond') loc
       e1' <- inferExpr vmap e1
       e2' <- inferExpr vmap e2
       unify (expr_type e1') (expr_type e2') loc
       derefExpr $ TcExpr (TcCondE cond' e1' e2') loc (expr_type e1')

inferExpr vmap (L loc (McLetE flag bind body))
  = do (vmap', bind') <- inferBind vmap bind False
       body' <- inferExpr vmap' body
       return $ TcExpr (TcLetE flag bind' body') loc (expr_type body')

inferExpr vmap (L loc (McTupleE elems))
  = do elems' <- mapM (inferExpr vmap) elems
       derefExpr $ TcExpr (TcTupleE elems') loc (TupleT (map expr_type elems'))

inferExpr vmap (L loc (McSeqE e1 e2))
  = do e1' <- inferExpr vmap e1
       e2' <- inferExpr vmap e2
       return $ TcExpr (TcSeqE e1' e2') loc (expr_type e2')


constType (IntC _)   = IntT
constType (FloatC _) = FloatT
constType (BoolC _)  = BoolT
constType UnitC      = UnitT

-- let binding
inferBind :: VarMap -> (LMcPat, LMcExpr) -> Bool
             -> TcM (VarMap, (TcPat, TcExpr))
inferBind vmap (L loc McAnyP, e) is_global
  = do e' <- inferExpr vmap e
       return (vmap, (TcPat TcAnyP loc (expr_type e'), e'))

inferBind vmap (L loc (McVarP name), e) is_global
  = do var_t <- return . TyVar =<< newTyId 
       var_id <- lift $ newUniq
       let vmap' = M.insert name 
                        (toScheme var_t, var_id, is_global) vmap
       e' <- inferExpr vmap' e
       unify var_t (expr_type e') loc
       scheme <- closure (expr_type e')
       let vmap'' = M.insert name (scheme, var_id, is_global) vmap'
       pat' <- derefPat $ 
          TcPat (TcVarP (mkVar (unName name) var_id scheme is_global)) loc (expr_type e')
       return (vmap'', (pat', e'))

inferBind vmap (tuplep, e) is_global
  = do (vmap', pat, t) <- parsePat vmap tuplep is_global
       e' <- inferExpr vmap' e
       unify (expr_type e') t (getLoc tuplep)
       pat' <- derefPat pat
       return (vmap', (pat', e'))

parsePat :: VarMap -> LMcPat -> Bool -> TcM (VarMap, TcPat, Type)
parsePat vmap (L loc McAnyP) _
  = do t <- return . TyVar =<< newTyId
       return (vmap, TcPat TcAnyP loc t, t)
parsePat vmap (L loc (McVarP name)) is_global
  = do var_t  <- return . TyVar =<< newTyId
       var_id <- lift $ newUniq
       return (M.insert name (toScheme var_t, var_id, is_global) vmap, 
               TcPat (TcVarP (mkVar (unName name) var_id (toScheme var_t) is_global)) loc var_t, var_t)
parsePat vmap (L loc (McTupleP pats)) is_global
  = walk vmap pats [] []
  where
  walk m [] ps' ts 
    = let tup_t = TupleT (reverse ts) 
       in return (m, TcPat (TcTupleP (reverse ps')) loc tup_t, tup_t)
  walk m (p:ps) ps' ts
    = do (m', p', t) <- parsePat m p is_global
         walk m' ps (p':ps') (t:ts)

{- Type unification -}
type UnifyM a = ErrorT Doc (StateT TcEnv ChocoM) a

unify :: Type -> Type -> SrcLoc -> TcM ()
unify t1 t2 loc
  = do c <- runErrorT (unifyM t1 t2)
       case c of
        Left msg -> lift $ compileError msg loc
        Right _  -> return ()

unifyM :: Type -> Type -> UnifyM ()
unifyM IntT    IntT    = return ()
unifyM FloatT  FloatT  = return ()
unifyM BoolT   BoolT   = return ()
unifyM UnitT   UnitT   = return ()

unifyM (ArrayT t1) (ArrayT t2) = unifyM t1 t2

unifyM (TupleT ts1) (TupleT ts2) = zipWithM_ unifyM ts1 ts2

unifyM (FunT args1 ret1) (FunT args2 ret2) =
  do zipWithM unifyM args1 args2
     unifyM ret1 ret2

unifyM t1 t2 = do
  env@TcEnv{ var_map = map } <- get
  case (t1, t2) of
    (TyVar i1, TyVar i2) 
      | i1 == i2   -> return ()

    (TyVar i1, t2) 
      | I.member i1 map
        -> let (TyScheme _ ty) = map I.! i1 
           in  unifyM ty t2

    (t1, TyVar i2)
      | I.member i2 map
        -> let (TyScheme _ ty) = map I.! i2
           in  unifyM t1 ty

    (TyVar i1, t2) 
      -> do occurCheck i1 t2
            t2s <- lift $ closure t2
            put env{ var_map = I.insert i1 t2s map }

    (t1, TyVar i2) 
      -> do occurCheck i2 t1
            t1s <- lift $ closure t1
            put env{ var_map = I.insert i2 t1s map }

    (t1, t2) 
      -> throwError $ text "Unification error:" <+> ppr t1 
          <+> text "<->" <+> ppr t2

{- Occurence check -}
occurCheck :: Id -> Type -> UnifyM ()
occurCheck id (FunT args ret) = do mapM (occurCheck id) args
                                   occurCheck id ret
occurCheck id (TupleT elems) = mapM_ (occurCheck id) elems
occurCheck id1 (TyVar id2) 
  | id1 == id2 = throwError $ text "infinite type"
  | otherwise  = do TcEnv{ var_map = map } <- get
                    case I.lookup id2 map of
                      Just scheme -> 
                        do (TyScheme _ ty) <- lift $ renameTySc scheme
                           occurCheck id1 ty
                      Nothing -> return ()
occurCheck _ _ = return ()

{- Dereference of type variables -}
derefType :: Type -> TcM Type
derefType (FunT args_t ret_t) 
  = do args_t' <- mapM derefType args_t
       ret_t'  <- derefType ret_t
       return $ FunT args_t' ret_t'

derefType (TupleT elems_t)
  = do elems_t' <- mapM derefType elems_t
       return $ TupleT elems_t'

derefType (ArrayT t)
  = do t' <- derefType t
       return $ ArrayT t'

derefType (TyVar id)
  = do env@TcEnv { var_map = m } <- get
       case I.lookup id m of
        Nothing -> return (TyVar id)
        Just (TyScheme ids ty)
          -> do ty' <- derefType ty
                scheme <- closure ty'
                put env{ var_map = I.insert id scheme m }
                return ty'

derefType t = return t

derefExpr :: TcExpr -> TcM TcExpr
derefExpr (TcExpr (TcVarE v) loc t)
  = do t' <- derefType t
       scheme <- closure t'
       return $ TcExpr (TcVarE v{ var_type = scheme }) loc t'

derefExpr (TcExpr (TcAppE f args) loc t)
  = do f' <- derefExpr f
       args' <- mapM derefExpr args
       t' <- derefType t
       return $ TcExpr (TcAppE f' args') loc t'

derefExpr (TcExpr (TcPrefixE op e) loc t)
  = do e' <- derefExpr e
       t' <- derefType t
       return $ TcExpr (TcPrefixE op e') loc t'

derefExpr (TcExpr (TcInfixE op e1 e2) loc t)
  = do e1' <- derefExpr e1
       e2' <- derefExpr e2
       t'  <- derefType t
       return $ TcExpr (TcInfixE op e1' e2') loc t'

derefExpr (TcExpr (TcLetE rec (pat, e) body) loc t)
  = do pat'  <- derefPat pat
       e'    <- derefExpr e
       body' <- derefExpr body
       t'    <- derefType t
       return $ TcExpr (TcLetE rec (pat', e') body') loc t'

derefExpr (TcExpr (TcFunE args body) loc t)
  = do args'  <- mapM derefPat args
       body'  <- derefExpr body
       t'     <- derefType t
       return $ TcExpr (TcFunE args' body') loc t'

derefExpr (TcExpr (TcTupleE elems) loc t)
  = do elems' <- mapM derefExpr elems
       t'     <- derefType t
       return $ TcExpr (TcTupleE elems') loc t'

derefExpr (TcExpr (TcCondE e1 e2 e3) loc t)
  = do e1' <- derefExpr e1
       e2' <- derefExpr e2
       e3' <- derefExpr e3
       t'  <- derefType t
       return $ TcExpr (TcCondE e1' e2' e3') loc t'

derefExpr (TcExpr (TcSeqE e1 e2) loc t)
  = do e1' <- derefExpr e1
       e2' <- derefExpr e2
       t'  <- derefType t
       return $ TcExpr (TcSeqE e1' e2') loc t'

derefExpr (TcExpr e loc t)
  = do t' <- derefType t
       return $ TcExpr e loc t'

derefPat :: TcPat -> TcM TcPat
derefPat (TcPat (TcVarP v) loc t)
  = do t' <- derefType t
       scheme <- closure t'
       return $ TcPat (TcVarP v{ var_type = scheme }) loc t'

derefPat (TcPat (TcTupleP pats) loc t)
  = do pats' <- mapM derefPat pats
       t'    <- derefType t
       return $ TcPat (TcTupleP pats') loc t'
derefPat (TcPat p loc t)
  = do t' <- derefType t
       return $ TcPat p loc t'

derefStmts :: [LTcStmt] -> TcM [LTcStmt]
derefStmts [] = return []
derefStmts (L loc (TcEvalS e) : rem)
  = do e' <- derefExpr e
       rem' <- derefStmts rem
       return $ L loc (TcEvalS e') : rem'

derefStmts (L loc (TcValueS (v, e)) : rem)
  = do v' <- derefPat v
       e' <- derefExpr e
       rem' <- derefStmts rem
       return $ L loc (TcValueS (v', e')) : rem'

{- Interface -}
typeInference :: [LMcStmt] -> TcM [LTcStmt]
typeInference ast 
  = do (_, tystmt) <- inferStmts (fst primTable) ast
       return tystmt
  
typeCheck :: [LMcStmt] -> ChocoM [LTcStmt]
typeCheck ast = evalStateT (typeInference ast) initTcEnv

dumpVarMap :: VarMap -> Doc
dumpVarMap tm
  = vcat $
    map (\(name, ty) -> ppr name <> char ':' <+> ppr ty) (M.toList tm)
