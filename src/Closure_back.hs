------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Closure (
  closeLambda
  ) where

import Choco
import Const
import LamSyn
import Panic
import Types
import Var

import Control.Monad.State
import Data.Bits
import qualified Data.Map as M

{- Closure conversion -}

type P a = StateT Env ChocoM a
type CEnv = M.Map Var Lambda
type FEnv = M.Map Var ValApprox
data Env = Env {
  fun_nest_depth  :: Int,
  fun_desc_table  :: M.Map Var FunDesc
  }

initEnv = Env {
  fun_nest_depth = 0,
  fun_desc_table = M.empty
  }

putFunDesc :: Var -> FunDesc -> P ()
putFunDesc id desc
  = modify $ \e@Env{ fun_desc_table = tbl } ->
      e{ fun_desc_table = M.insert id desc tbl }

getFunDesc :: Var -> P FunDesc
getFunDesc id
  = do tbl <- gets fun_desc_table 
       case M.lookup id tbl of
         Just desc -> return desc
         Nothing   -> panic "closure conversion error"

type VarTable = M.Map Var Lambda

data ValApprox
  = ValClosure Var ValApprox
  | ValTuple [ValApprox]
  | ValInt Int
  | ValFloat Float
  | ValBool Bool
  | ValConstPtr Int
  | ValUnknown

data FunDesc = FunDesc {
  fun_label   :: Var,
  fun_closed  :: Bool,
  fun_inline  :: Maybe ([Var], Lambda)
  }

close :: FEnv -> CEnv -> Lambda -> P (Lambda, ValApprox)
close fenv cenv (Lvar v) = return $ closeApproxVar fenv cenv v
close fenv cenv (Llit c)
  = case c of
    IntC n     -> return (Llit c, ValInt n)
    FloatC f   -> return (Llit c, ValFloat f)
    BoolC b    -> return (Llit c, ValBool b)
    PointerC n -> return (Llit c, ValConstPtr n)
    _          -> return (Llit c, ValUnknown)
close fenv cenv f@(Lfun args body)
  = do id <- lift $ mkTmpVar "fun" UnknownT
       closeOneFunction fenv cenv id f

close fenv cenv (Lapp f args)
  = do (f', ValClosure id approx_res) <- close fenv cenv f
       args' <- closeList fenv cenv args
       fdesc <- getFunDesc id
       app <- directApply fdesc f f' args'
       return (app, approx_res) -- ToDo

close fenv cenv (Llet (v, e) body)
  = do (e', appe) <- case e of
                     Lfun params body -> closeOneFunction fenv cenv v e
                     _ -> close fenv cenv e
       (body', appbody) <- close (M.insert v appe fenv) cenv body
       return (Llet (v, e') body', appbody)

close fenv cenv (Lseq e1 e2)
  = do (e1', _) <- close fenv cenv e1
       (e2', approx) <- close fenv cenv e2
       return (Lseq e1' e2', approx)

close fenv cenv (Lcond e1 e2 e3)
  = do ret <- close fenv cenv e1
       case ret of
        (e1', ValBool True)
          -> if isPureLambda e1'
               then close fenv cenv e2
               else do (e2', approx) <- close fenv cenv e2
                       return $ (Lseq e1' e2', approx)
        (e1', ValBool False)
          -> if isPureLambda e1'
               then close fenv cenv e3
               else do (e3', approx) <- close fenv cenv e3
                       return $ (Lseq e1' e3', approx)
        (e1', _)
          -> do (e2', _) <- close fenv cenv e2
                (e3', _) <- close fenv cenv e3
                return $ (Lcond e1' e2' e3', ValUnknown)

close fenv cenv (Lprim p args)
  = do args' <- mapM (close fenv cenv) args
       return $ simplifPrim p (unzip args')

closeList fenv cenv [] = return []
closeList fenv cenv (lam:rem)
  = do (lam', _) <- close fenv cenv lam
       rem' <- closeList fenv cenv rem
       return $ lam' : rem'

closeApproxVar fenv cenv id
  = case M.findWithDefault ValUnknown id fenv of
      ValInt n      -> (Llit (IntC n),   ValInt n)
      ValFloat f    -> (Llit (FloatC f), ValFloat f)
      ValConstPtr n -> (Llit (PointerC n), ValConstPtr n)
      approx -> let subst = M.findWithDefault (Lvar id) id cenv in
                (subst, approx)

closeVar fenv cenv id = fst $ closeApproxVar fenv cenv id

excessiveFunctionNestDepth = 5

closeOneFunction fenv cenv id f
  = do ret <- closeFunction fenv cenv id f
       case ret of
        (clos@(Lclosure (_, params, body) _),
         (_, _, approx@(ValClosure id _))) ->
          do threshold <- lift $ getFlag inline_threshold
             when (lambdaSmaller body (threshold + length params)) $
               do fdesc <- getFunDesc id
                  putFunDesc id fdesc{ fun_inline = Just (params, body) }
             return (clos, approx)

closeFunction :: FEnv -> CEnv -> Var -> Lambda 
  -> P (Lambda, (Var, Int, ValApprox))
closeFunction fenv cenv id fun@(Lfun params body)
  = do modify (\e@Env{ fun_nest_depth = n } -> e{ fun_nest_depth = n+1 })
       nest <- gets fun_nest_depth
       let initially_closed = nest < excessiveFunctionNestDepth 

       {- Determine the free variables of the functions -}
       let fv = freeVars (Llet (id, fun) (Llit UnitC))

       {- build function descriptor -}
       let fdesc = FunDesc id initially_closed Nothing
       modify $ \e@Env{ fun_desc_table = tbl } ->
          e{ fun_desc_table = M.insert id fdesc tbl}

       {- build an approximate fenv for compiling the functions -}
       let fenv_rec = M.insert id (ValClosure id ValUnknown) fenv

       (clos, info) <- do 
        if initially_closed 
          then do (cl, useless_env) <- 
                    close_fundef id params body fenv_rec fv
                  if useless_env 
                    then return cl
                    else do fundesc <- getFunDesc id
                            putFunDesc id fundesc{ fun_closed = False }
                            ret <- close_fundef id params body fenv_rec fv
                            return $ fst ret
          else do ret <- close_fundef id params body fenv_rec fv
                  return $ fst ret

       let fv' = map (closeVar fenv cenv) fv
       return $ (Lclosure clos fv', info)

  where
  close_fundef id params body fenv fv
    = do
      env_param <- lift $ mkTmpVar "env" UnknownT
      let cenv_fv = buildClosureEnv env_param 0 fv
      let cenv_body = M.insert id (Loffset (Lvar env_param) 0) cenv_fv
      (ubody, approx) <- close fenv cenv_body body
      let useless_env = not (occursVar env_param ubody)
      let  params' = if useless_env then params else params ++ [env_param]
      return $ (((id, params', ubody),
                   (id, 0, ValClosure id approx)), useless_env)


buildClosureEnv env_param pos [] = M.empty
buildClosureEnv env_param pos (id:rem)
  = M.insert id (Lprim (Pfield pos) [Lvar env_param])
      (buildClosureEnv env_param (pos+1) rem)

directApply fdesc funct cfunct cargs
  = do
    let app_args = if fun_closed fdesc then cargs else cargs ++ [cfunct]
    app <- 
      case fun_inline fdesc of
        Nothing -> return $ Lapp (Lvar (fun_label fdesc)) app_args
        Just (params, body)
          -> bindParams params app_args body
    if not (fun_closed fdesc) || isPureLambda cfunct
      then return app
      else return $ Lseq cfunct app

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
                  then return $ Llet (p', a) body'
                  else if noEffects a
                    then return body'
                    else return $ Lseq a body'
  iter _ ps as _ = panic $ "bindParams" ++ show ps ++ show as

substitute subst lam
  = case lam of
    Lvar v -> return $ M.findWithDefault lam v subst
    Llit _ -> return lam
    Lapp (Lvar v) args 
      -> do args' <- mapM (substitute subst) args
            return $ Lapp (Lvar v) args'
    Lapp f args 
      -> do f' <- substitute subst f
            args' <- mapM (substitute subst) args
            return $ Lapp f' args'
    Lclosure def env 
      -> do env' <- mapM (substitute subst) env
            return $ Lclosure def env'
    Loffset lam ofs 
      -> do lam' <- substitute subst lam
            return $ Loffset lam' ofs
    Llet (id, e) body 
      -> do id' <- lift $ mkTmpVar (var_name id) (var_type id)
            e' <- substitute subst e
            body' <- substitute (M.insert id (Lvar id') subst) body
            return $ Llet (id', e') body'
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

approxLam (Llit (IntC n))     = ValInt n
approxLam (Llit (FloatC f))   = ValFloat f
approxLam (Llit (BoolC b))    = ValBool b
approxLam (Llit (PointerC p)) = ValConstPtr p
approxLam _                   = ValUnknown

makeConstInt n   = (Llit (IntC n), ValInt n)
makeConstFloat f = (Llit (FloatC f), ValFloat f)
makeConstBool b  = (Llit (BoolC b), ValBool b)

simplifPrim p (args, approxs)
  | all isPureLambda args
    = case approxs of
      [ValInt n] ->
        case p of
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
  | otherwise
    = (Lprim p args, ValUnknown)

{- Interface -}
closeLambda :: Lambda -> ChocoM Lambda
closeLambda lam
  = do (lam, _) <- evalStateT (close M.empty M.empty lam) initEnv
       return lam
