------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module ElimLet (
  simplifyLets
  ) where

import Choco
import Const
import Id
import LamSyn
import Outputable
import Panic
import Var

import Control.Monad.State
import qualified Data.IntMap as I
import qualified Data.Map as M
import Data.Maybe

type P a = StateT Env ChocoM a

data Env = Env {
  -- table of occurrence count and read-only flag 
  occ_table  :: M.Map Var (Int, Bool), 

  subst_map  :: M.Map Var Lambda
  }

initEnv = Env {
  occ_table = M.empty,
  subst_map = M.empty
  }

incrVar v n = do
  e@Env{ occ_table = map } <- get
  let (c, r) = M.findWithDefault (0, True) v map
  put e{ occ_table = M.insert v (c+n, r) map }

getVarCount v = do
  e@Env{ occ_table = map } <- get
  return $ M.findWithDefault (0, True) v map

notReadonly v = do
  e@Env{ occ_table = map } <- get
  let (c, _) = M.findWithDefault (0, panic "notReadonly") v map
  put e{ occ_table = M.insert v (c, False) map }

countVars :: Lambda -> P ()
countVars (Lvar v) = incrVar v 1
countVars (Llit _) = return ()
countVars (Lapp f args _) = mapM_ countVars (f:args)
countVars (Lfun _ body) = countVars body
countVars (Llet _ (v, Lvar w) e)
  {- v will be replaced by w in e, so each occurence of v in e
   - increases w's refcount -}
  = do countVars e
       (vc, vr) <- getVarCount v
       e@Env{ occ_table = occ } <- get
       case M.lookup w occ of
          Just (c, r) -> 
            put e{ occ_table = M.insert w (c + vc, r && vr) occ }
          Nothing -> put e{ occ_table = M.insert w (vc, vr) occ }
countVars (Llet str (v, e) body)
  = do countVars body
       (vc, _) <- getVarCount v
       when (str == Strict || vc > 1) (countVars e)
countVars (Lletrec (v, e) body) = mapM_ countVars [e,body]
countVars (Lprim _ args)        = mapM_ countVars args
countVars (Lcond e1 e2 e3)      = mapM_ countVars [e1,e2,e3]
countVars (Lseq e1 e2)          = mapM_ countVars [e1,e2]
countVars (Lassign v e)         = do { notReadonly v; countVars e }
countVars (Lfor v beg end body)
  = do { notReadonly v; countVars beg; countVars end; countVars body }
countVars (Lwhile e body)
  = do { countVars e; countVars body }


addSubst :: Var -> Lambda -> P ()
addSubst v w 
  = modify $ \e -> e{ subst_map = M.insert v w (subst_map e) }

elimLet :: Lambda -> P Lambda
elimLet l@(Lvar v) 
  = do Env{ subst_map = map } <- get
       return $ M.findWithDefault l v map

elimLet l@(Llit c) = return l
elimLet (Lapp f args p)
  = do f'    <- elimLet f
       args' <- mapM elimLet args
       return $ Lapp f' args' p
elimLet (Lfun args body)
  = do body' <- elimLet body
       return $ Lfun args body'
elimLet (Llet Strict (v, Lvar w) e)
  = do w' <- elimLet (Lvar w)
       addSubst v w'
       elimLet e
elimLet (Llet str (v, e) cont)
  | hasSideEffect e
  = do e' <- elimLet e
       cont' <- elimLet cont
       return $ Llet str (v, e') cont'
elimLet (Llet Strict (v, e) cont)
  = do (vc, _) <- getVarCount v
       case vc of
         0 -> do lift$putLog$ text "\teliminate unused" <+> ppr v
                 elimLet cont
         1 -> do e' <- elimLet e
                 addSubst v e'
                 lift$putLog$ text "\tinline" <+> ppr v
                 elimLet cont
         _ -> do e'    <- elimLet e
                 cont' <- elimLet cont
                 return $ Llet Strict (v, e') cont'
elimLet (Llet str (v, e) cont)
  = do e' <- elimLet e
       cont' <- elimLet cont
       r <- getVarCount v
       case r of
        -- (0, _)     -> return cont'
        (_, True)  -> return $ Llet Strict (v, e') cont'
        (_, False) -> return $ Llet Variable (v, e') cont'
elimLet (Lletrec (v, e) cont)
  = do e' <- elimLet e
       cont' <- elimLet cont
       return $ Lletrec (v, e') cont'
elimLet (Lprim p args)
  = do args' <- mapM elimLet args
       return $ Lprim p args'
elimLet (Lcond e1 e2 e3)
  = do e1' <- elimLet e1
       e2' <- elimLet e2
       e3' <- elimLet e3
       return $ Lcond e1' e2' e3'
elimLet (Lseq e1 e2)
  = do e1' <- elimLet e1
       e2' <- elimLet e2
       return $ Lseq e1' e2'
elimLet (Lwhile e1 e2)
  = do e1' <- elimLet e1
       e2' <- elimLet e2
       return $ Lwhile e1' e2'
elimLet (Lfor v e1 e2 e3)
  = do e1' <- elimLet e1
       e2' <- elimLet e2
       e3' <- elimLet e3
       return $ Lfor v e1' e2' e3'
elimLet (Lassign v e)
  = do (vc, _) <- getVarCount v
       case vc of
          0 -> return $ Llit UnitC
          _ -> do
            e' <- elimLet e
            return $ Lassign v e'

{- Interface -}
simplifyLets :: Lambda -> ChocoM Lambda
simplifyLets lam = evalStateT (countVars lam >> elimLet lam) initEnv
