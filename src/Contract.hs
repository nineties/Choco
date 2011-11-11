------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Contract (
  doContract
  ) where

import Choco
import Const
import LamSyn
import Outputable
import Panic
import Primitive
import Var

import Control.Monad.State
import Data.Bits
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S

type P a = StateT Env ChocoM a

data Env = Env {
  inline_funs   :: M.Map Var ([Var], Lambda),
  var_env       :: M.Map Var ValApprox,
  lam_env       :: M.Map Var Lambda,
  global_approx :: M.Map Var ValApprox,
  destroyed :: M.Map Var (S.Set Var),
  current_fun   :: Maybe Var
  }

initEnv = Env {
  inline_funs   = M.empty,
  var_env       = M.empty,
  lam_env       = M.empty,
  global_approx = M.empty,
  destroyed = M.empty,
  current_fun   = Nothing
  }

data ValApprox
  = ValFun ValApprox
  | ValTuple [ValApprox]
  | ValInt Int
  | ValFloat Float
  | ValBool Bool
  | ValConstPtr Int
  | ValUnknown
  deriving (Show, Eq)

doContract lam = evalStateT (return . fst =<< contract lam) initEnv

contract :: Lambda -> P (Lambda, ValApprox)
contract (Lvar v) 
  | isGlobal v = do
    table <- gets global_approx
    case M.lookup v table of
      Just (ValInt n)   -> return (Llit (IntC n), ValInt n)
      Just (ValFloat f) -> return (Llit (FloatC f), ValFloat f)
      Just (ValBool b)  -> return (Llit (BoolC b), ValBool b)
      _ -> approxVar v
  | otherwise = approxVar v
contract (Llit c) = case c of
    IntC n     -> return (Llit c, ValInt n)
    FloatC f   -> return (Llit c, ValFloat f)
    BoolC b    -> return (Llit c, ValBool b)
    PointerC n -> return (Llit c, ValConstPtr n)
    _          -> return (Llit c, ValUnknown)

contract (Lfun args body) 
  = do (body', approx) <- contract body
       return (Lfun args body', ValFun approx)

contract (Lapp f args p)
  = do (f', approx)    <- contract f
       args' <- mapM contract args
       app <- genApply f' (fst $ unzip args') p
       case app of
        Lapp (Lvar id) _ _ -> do
            f <- gets current_fun
            set1 <- return . M.findWithDefault S.empty id =<< gets destroyed
            case f of
              Just f -> do
                set2 <- return . M.findWithDefault S.empty f =<< gets destroyed
                modify $ \e -> e{
                  destroyed = M.insert f (set1 `S.union` set2) (destroyed e)
                  }
              Nothing -> return ()

            apps <- gets global_approx
            let apps' = foldr M.delete apps (S.toList set1)
            modify $ \e -> e{ global_approx = apps' }

        _ -> return() -- this function is inlined
       return (app, ValUnknown)

contract (Llet Strict (id, Lfun params body) cont)
  = do threshold <- lift $ getFlag inline_threshold
       gsave <- gets global_approx
       fsave <- gets current_fun
       modify $ \e -> e{ current_fun = Just id, global_approx = M.empty }

       (body', body_approx) <- contract body

       modify $ \e -> e{ current_fun = fsave, global_approx = gsave }
       if lambdaSmaller body' (threshold + length params)
          then do modify $ \e -> e{ inline_funs = 
                     M.insert id (params, body') (inline_funs e) }
                  lift$putLog$ text "\tinline" <+> ppr id
                  contract cont
          else do modify $ \e -> e{ var_env = M.insert id body_approx (var_env e) }
                  (cont', cont_approx) <- contract cont
                  return (Llet Strict (id, Lfun params body') cont', cont_approx)

contract (Llet str (id, e) cont)
  = do (e', e_approx) <- contract e
       when (str == Strict) $ 
          modify $ \env -> 
              env{ var_env = M.insert id e_approx (var_env env) }
       (cont', cont_approx) <- contract cont
       return (Llet str (id, e') cont', cont_approx)

contract (Lletrec (id, e@(Lfun params body)) cont)
  = do gsave <- gets global_approx
       fsave <- gets current_fun

       modify $ \e -> e{ current_fun = Just id, global_approx = M.empty }
       (e', e_approx)    <- contract e
       modify $ \e -> e{ 
        current_fun = fsave, 
        global_approx = gsave,
        var_env = M.insert id e_approx (var_env e)
        }
       (cont', cont_approx) <- contract cont
       return (Lletrec (id, e') cont', cont_approx)

contract (Lprim p args)
  = do args' <- mapM contract args
       return $ simplifPrim p (unzip args')

contract (Lcond e1 e2 e3)
  = do test <- contract e1
       case test of
        (e1', ValBool True)
          -> if isPureLambda e1'
              then contract e2
              else do (e2', app) <- contract e2
                      return (Lseq e1' e2', app)
        (e1', ValBool False)
          -> if isPureLambda e1'
              then contract e3
              else do (e3', app) <- contract e3
                      return (Lseq e1' e3', app)
        (e1', _)
          -> do (e2', _) <- contract e2
                (e3', _) <- contract e3
                return (Lcond e1' e2' e3', ValUnknown)

contract (Lseq e1 e2)
  = do (e1', _) <- contract e1
       (e2', app) <- contract e2
       return (Lseq e1' e2', app)

contract (Lwhile e1 e2) 
  = do (e1', _) <- contract e1
       (e2', _) <- contract e2
       return (Lwhile e1' e2', ValUnknown)

contract (Lfor v beg end body) 
  = do (beg', _) <- contract beg
       (end', _) <- contract end
       (body', _) <- contract body
       return (Lfor v beg' end' body', ValUnknown)

contract (Lassign v e)
  = do (e', app) <- contract e
       modify $ \e -> e{ global_approx = M.insert v app (global_approx e) }

       id <- gets current_fun
       case id of
        Just id -> do
          set <- return . M.findWithDefault S.empty id =<< gets destroyed
          modify $ \e -> e{ 
           destroyed = M.insert id (S.insert v set) (destroyed e)
           }
        Nothing -> return ()
       return (Lassign v e', ValUnknown)

genApply (Lvar v) args p
  = do table <- gets inline_funs
       case M.lookup v table of
          Just (params, body) -> bindParams params args body
          Nothing             -> return $ Lapp (Lvar v) args p
genApply (Lfun params body) args _
  = bindParams params args body
genApply f args p
  = lift $ simpleError 
      (text "invalid application:"<+> ppr (Lapp f args p))

bindParams params args body
  = iter M.empty params args body
  where
  iter subst [] [] body = substitute subst body
  iter subst (p:ps) (a:as) body
    = if isSimpleArgument a
        then iter (M.insert p a subst) ps as body
        else do p' <- lift $ mkTmpVar (var_name p) (var_type p)
                body' <- iter (M.insert p (Lvar p') subst) ps as body
                if occursVar p body
                  then return $ Llet Strict (p', a) body'
                  else if hasSideEffect a
                    then return $ Lseq a body'
                    else return body'
  iter _ _ _ _ = panic $ "bindParams"

substitute subst lam
  = case lam of
    Lvar v -> return $ M.findWithDefault lam v subst
    Llit _ -> return lam
    Lapp (Lvar v) args p
      -> do args' <- mapM (substitute subst) args
            return $ Lapp (Lvar v) args' p
    Lapp f args p
      -> do f' <- substitute subst f
            args' <- mapM (substitute subst) args
            return $ Lapp f' args' p
    Llet str (id, e) body 
      -> do id' <- lift$copyVar id
            e' <- substitute subst e
            body' <- substitute (M.insert id (Lvar id') subst) body
            return $ Llet str (id', e') body'
    Lletrec (id, e) body
      -> do id' <- lift$copyVar id
            e'  <- substitute subst e
            body' <- substitute (M.insert id (Lvar id') subst) body
            return $ Lletrec (id', e') body'
    Lprim p args
      -> do args' <- mapM (substitute subst) args
            let (res, _) = simplifPrim p (args', map approxLam args')
            return res
    Lcond e1 e2 e3
      -> do e1' <- substitute subst e1
            case e1' of
              Llit (BoolC True) -> substitute subst e2
              Llit (BoolC False) -> substitute subst e3
              _ -> do e2' <- substitute subst e2
                      e3' <- substitute subst e3
                      return $ Lcond e1' e2' e3'
    Lseq e1 e2
      -> do e1' <- substitute subst e1
            e2' <- substitute subst e2
            return $ Lseq e1' e2'

    Lwhile e1 e2
      -> do e1' <- substitute subst e1
            e2' <- substitute subst e2
            return $ Lwhile e1' e2'

    Lfor v beg end body
      -> do v' <- lift$copyVar v
            beg' <- substitute subst beg
            end' <- substitute subst end
            body' <- substitute (M.insert v (Lvar v') subst) body
            return $ Lfor v' beg' end' body'

    Lassign v e
      -> do e' <- substitute subst e
            return $ Lassign v e'

{- constant folding -}
approxVar id
  = do Env{ var_env = venv, lam_env = lenv } <- get
       case M.findWithDefault ValUnknown id venv of
        ValInt n      -> return (Llit (IntC n),   ValInt n)
        ValFloat f    -> return (Llit (FloatC f), ValFloat f)
        ValBool b     -> return (Llit (BoolC b), ValBool b)
        ValConstPtr n -> return (Llit (PointerC n), ValConstPtr n)
        approx -> let subst = M.findWithDefault (Lvar id) id lenv
                  in return (subst, approx)

approxLam (Llit (IntC n))     = ValInt n
approxLam (Llit (FloatC f))   = ValFloat f
approxLam (Llit (BoolC b))    = ValBool b
approxLam (Llit (PointerC p)) = ValConstPtr p
approxLam _                   = ValUnknown

makeConstInt n   = (Llit (IntC n), ValInt n)
makeConstFloat f = (Llit (FloatC f), ValFloat f)
makeConstBool b  = (Llit (BoolC b), ValBool b)

simplifPrim PcreateTuple (args, approxs) 
  = (Lprim PcreateTuple args, ValTuple approxs)
simplifPrim (PtupleRef n) ([arg], [ValTuple elems])
  = case elems !! n of
    ValInt n    -> makeConstInt n
    ValFloat f  -> makeConstFloat f
    ValBool b   -> makeConstBool b
    _           -> (Lprim (PtupleRef n) [arg], elems !! n)
simplifPrim PcreateArray ([Llit (IntC 0), _], _)
  = (Llit nullPtr, ValUnknown)
simplifPrim p (args, approxs)
  | all isPureLambda args
    = case approxs of
      [ValInt n] ->
        case p of
          Pextcall s _ _ | s == extPrefix ++ "print_int"
            -> (let digits = map ord (show n) in
               foldr (\n cont -> Lseq (Lprim Pput [Llit (IntC n)]) cont)
                (Llit UnitC) digits, ValUnknown)
          Pnegi -> makeConstInt (-n)
          Pitof -> makeConstFloat (fromIntegral n)
          _ -> (Lprim p args, ValUnknown)
      [ValInt x, ValInt y] ->
        case p of
          Paddi -> makeConstInt (x + y)
          Psubi -> makeConstInt (x - y)
          Pmuli -> makeConstInt (x * y)
          Pdivi -> makeConstInt (x `div` y)
          Plsl  -> makeConstInt (x `shiftL` y)
          Pasr  -> makeConstInt (x `shiftR` y)
          Pcompi Ceq -> makeConstBool (x == y)
          Pcompi Cne -> makeConstBool (x /= y)
          Pcompi Clt -> makeConstBool (x < y)
          Pcompi Cgt -> makeConstBool (x > y)
          Pcompi Cle -> makeConstBool (x <= y)
          Pcompi Cge -> makeConstBool (x >= y)
          _ -> (Lprim p args, ValUnknown)
      [ValFloat f] ->
        case p of
          Pftoi -> makeConstInt (floor f)
          Pabsf -> makeConstFloat (abs f)
          Pnegf -> makeConstFloat (negate f)
          Psqrt -> makeConstFloat (sqrt f)
          _ -> (Lprim p args, ValUnknown)
      [ValFloat x, ValFloat y] ->
        case p of
          Paddf -> makeConstFloat (x + y)
          Psubf -> makeConstFloat (x - y)
          Pmulf -> makeConstFloat (x * y)
          Pdivf -> makeConstFloat (x / y)
          Pcompf Ceq -> makeConstBool (x == y)
          Pcompf Cne -> makeConstBool (x /= y)
          Pcompf Clt -> makeConstBool (x < y)
          Pcompf Cgt -> makeConstBool (x > y)
          Pcompf Cle -> makeConstBool (x <= y)
          Pcompf Cge -> makeConstBool (x >= y)
          _ -> (Lprim p args, ValUnknown)
      [ValBool b] ->
        case p of
          Pnot -> makeConstBool (not b)
          _ -> (Lprim p args, ValUnknown)
      _ -> (Lprim p args, ValUnknown)
simplifPrim p (args, approxs) = (Lprim p args, ValUnknown)
