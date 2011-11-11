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
import LamSyn hiding (occursVar)
import CLamSyn 
import Outputable
import Panic
import Types
import Var

import Control.Monad.State
import Data.Bits
import qualified Data.Map as M
import qualified Data.Set as S

{- Closure conversion -}

type P a = StateT Env ChocoM a
data Env = Env {
  fun_nest_depth  :: Int,
  fun_desc_table  :: M.Map Var FunDesc
  }
  deriving (Show)

initEnv = Env {
  fun_nest_depth = 0,
  fun_desc_table = M.empty
  }

type FMap = M.Map Var (Maybe Var)
type CMap = M.Map Var CLambda

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

modifyFunDesc :: Var -> (FunDesc -> FunDesc) -> P ()
modifyFunDesc v f = getFunDesc v >>= putFunDesc v . f

type VarTable = M.Map Var Lambda

data FunDesc = FunDesc {
  fun_label   :: Var,
  fun_closed  :: Bool
  }
  deriving (Show)

close :: FMap -> CMap -> Lambda -> P (CLambda, Maybe Var)
close fenv cenv (Lvar v) = return $ closeVar fenv cenv v
close fenv cenv (Llit c) = return (Ulit c, Nothing)
close fenv cenv lam@(Lfun params body) 
  = do f <- lift$mkTmpVar "fun" (toScheme UnknownT)
       (clos, id) <- closeFunction fenv cenv f lam
       return (clos, Just id)

close fenv cenv (Lapp f args p)
  = do (f', approx) <- close fenv cenv f
       case approx of
        Just label -> do
          args' <- mapM (close fenv cenv) args
          fdesc <- getFunDesc label
          app <- directApply fdesc f' label (fst.unzip $ args') p
          return (app, Nothing)
        Nothing -> lift$simpleError 
          (text "Choco doesn't support direct function application")

close fenv cenv (Llet _ (v, f@(Lfun _ _)) body)
  = do (clos, id) <- closeFunction fenv cenv v f
       (body', _) <- close (M.insert v (Just id) fenv) cenv body
       return (Ulet (v, clos) body', Just id)

close fenv cenv (Llet str (v, e) body)
  = do 
    (e', app) <- close fenv cenv e
    (body', _) <- close (M.insert v app fenv) cenv body
    return (Ulet (v, e') body', Nothing)

close fenv cenv (Lletrec (v, f@(Lfun _ _)) body)
  = do (clos, id) <- closeFunction fenv cenv v f
       clos_ident <- lift$mkTmpVar "clos" (toScheme UnknownT)
       (body', _) <- close (M.insert v (Just id) fenv) cenv body
       body'' <- substitute (M.singleton id (Uoffset (Uvar clos_ident) 0)) body'
       return (Ulet (clos_ident, clos) body'', Just id)

close fenv cenv (Lprim p args) 
  = do args' <- mapM (close fenv cenv) args
       return (Uprim p (fst.unzip $ args'), Nothing)

close fenv cenv (Lcond e1 e2 e3)
  = do (e1', _) <- close fenv cenv e1
       (e2', _) <- close fenv cenv e2
       (e3', _) <- close fenv cenv e3
       return (Ucond e1' e2' e3', Nothing)

close fenv cenv (Lseq e1 e2)
  = do (e1', _) <- close fenv cenv e1
       (e2', e2_app) <- close fenv cenv e2
       return (Useq e1' e2', e2_app)

close fenv cenv (Lwhile e1 e2)
  = do (e1', _) <- close fenv cenv e1
       (e2', _) <- close fenv cenv e2
       return (Uwhile e1' e2', Nothing)

close fenv cenv (Lfor v e1 e2 e3)
  = do (e1', _) <- close fenv cenv e1
       (e2', _) <- close fenv cenv e2
       (e3', _) <- close fenv cenv e3
       return (Ufor v e1' e2' e3', Nothing)
close fenv cenv (Lassign v e)
  = do (e', _) <- close fenv cenv e
       return (Uassign v e', Nothing)

excessiveFunctionNestDepth = 5

closeVar :: FMap -> CMap -> Var -> (CLambda, Maybe Var)
closeVar fenv cenv v = 
  (M.findWithDefault (Uvar v) v cenv, 
    M.findWithDefault Nothing v fenv)

closeFunction :: FMap -> CMap -> Var -> Lambda -> P (CLambda, Var)
closeFunction fenv cenv id fun@(Lfun params body)
  = do nest <- return . (+ 1) =<< gets fun_nest_depth
       modify $ \e -> e{ fun_nest_depth = nest }
       let initially_closed = nest < excessiveFunctionNestDepth

       {- Determine the free variables of the functions -}
       let fv = filter (not.isGlobal) . S.toList $ 
                  freeVars (Lletrec (id, fun) (Llit UnitC))

       {- Build function descriptor -}
       let fdesc = FunDesc {
        fun_label = id, fun_closed = initially_closed 
        }
       putFunDesc id fdesc

       (clos, info) <- do
          if initially_closed
            then do 
              (cl, useless_env) <- close_fundef id params body fv
              if useless_env
                then return cl
                else do
                  modifyFunDesc id $ \f -> f{ fun_closed = False }
                  ret <- close_fundef id params body fv
                  return $ fst ret
            else do
              ret <- close_fundef id params body fv
              return $ fst ret
       
       let fv' = fst.unzip $ map (closeVar fenv cenv) fv

       modify $ \e -> e{ fun_nest_depth = nest - 1 }
       return $ (Uclosure clos fv', info)
  where
  close_fundef id params body fv
    = do
      env_param <- lift$mkTmpVar "env" (toScheme UnknownT)
      let cenv_fv = buildClosureEnv env_param 0 fv
      let cenv_body = M.insert id (Uoffset (Uvar env_param) 0) cenv_fv

      let fenv_rec = M.insert id (Just id) fenv
      (ubody,_) <- close fenv_rec cenv_body body
      let useless_env = not (occursVar env_param ubody)
      let params' = if useless_env then params else params ++ [env_param]
      return (((id, params', ubody), id), useless_env)

buildClosureEnv env_param pos [] = M.empty
buildClosureEnv env_param pos (id:rem)
  = M.insert id (Uprim (Pfield pos) [Uvar env_param])
      (buildClosureEnv env_param (pos+1) rem)

directApply fdesc fun cfun cargs p
  = do
    let app_args = if fun_closed fdesc then cargs else cargs ++ [Uvar cfun]
    return $ Uapply (fun_label fdesc) app_args p


substitute subst lam
  = case lam of
    Uvar v -> return $ M.findWithDefault lam v subst
    Ulit _ -> return lam
    Uapply v args p
      -> do args' <- mapM (substitute subst) args
            return $ Uapply v args' p
    Ulet (id, e) body 
      -> do id' <- lift$copyVar id
            e' <- substitute subst e
            body' <- substitute (M.insert id (Uvar id') subst) body
            return $ Ulet (id', e') body'
    Uprim p args
      -> do args' <- mapM (substitute subst) args
            return $ Uprim p args'
    Ucond e1 e2 e3
      -> do e1' <- substitute subst e1
            case e1' of
              Ulit (BoolC True) -> substitute subst e2
              Ulit (BoolC False) -> substitute subst e3
              _ -> do e2' <- substitute subst e2
                      e3' <- substitute subst e3
                      return $ Ucond e1' e2' e3'
    Useq e1 e2
      -> do e1' <- substitute subst e1
            e2' <- substitute subst e2
            return $ Useq e1' e2'

    Uwhile e1 e2
      -> do e1' <- substitute subst e1
            e2' <- substitute subst e2
            return $ Uwhile e1' e2'

    Ufor v e1 e2 e3
      -> do v' <- lift$copyVar v
            e1' <- substitute subst e1
            e2' <- substitute subst e2
            e3' <- substitute (M.insert v (Uvar v') subst) e3
            return $ Ufor v' e1' e2' e3'

    Uassign v e
      -> do e' <- substitute subst e
            return $ Uassign v e'

    Uclosure def clos 
      -> do clos' <- mapM (substitute subst) clos
            return $ Uclosure def clos'

    Uoffset e n
      -> do e' <- substitute subst e
            return $ Uoffset e' n



{- Interface -}
closeLambda :: Lambda -> ChocoM CLambda
closeLambda lam = return . fst =<< evalStateT (close M.empty M.empty lam) initEnv
