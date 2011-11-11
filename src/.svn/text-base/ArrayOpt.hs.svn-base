------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module ArrayOpt (
  arrayOpt
  ) where

import Choco
import Const
import LamSyn
import Outputable
import Var
import Types

import Control.Monad.State
import qualified Data.Set as S


simplif :: Lambda -> ChocoM Lambda
simplif lam = case lam of
  Lvar _ -> return lam
  Llit _ -> return lam
  Lapp f args p -> do 
    f' <- simplif f
    args' <- mapM simplif args
    return $ Lapp f' args' p

  Lfun args body -> do
    body' <- simplif body
    return $ Lfun args body'

  Llet Strict (ary, Lprim PcreateArray args) body -> do
    args' <- mapM simplif args
    case head args' of
      Llit (IntC n) -> do
        ids <- replicateM n newUniq
        let names = map (\i -> var_name ary ++ show i) [0..]
        let vars = zipWith 
              (\i n -> mkVar n i (var_type ary) (isGlobal ary)) ids names
        case eliminateArray ary vars body of
          Just body' | isSimpleArgument (args' !! 1)
            -> do
            putLog (text "\tchange array" <+> ppr ary <+> text "to variable(s)")
            body'' <- simplif body'
            return $ foldr 
              (\v cont -> Llet Variable (v, args'!!1) cont)
                    body'' vars
          Just body' -> do 
            putLog (text "\tchange array" <+> ppr ary <+> text "to variable(s)")
            v' <- copyVar ary
            body'' <- simplif body'
            return$ Llet Strict (v', args' !! 1) $ 
              foldr (\v cont -> Llet Variable (v, Lvar v') cont)
                    body'' vars
          Nothing -> do
            body' <- simplif body
            return $ Llet Strict (ary, Lprim PcreateArray args') body'
      _ -> do 
        body' <- simplif body
        return $ Llet Strict (ary, Lprim PcreateArray args') body'
  Llet str (v, e) cont -> do
    e' <- simplif e
    cont' <- simplif cont
    return $ Llet str (v, e') cont'
  Lletrec (v, e) cont -> do
    e' <- simplif e
    cont' <- simplif cont
    return $ Lletrec (v, e') cont'
  Lprim p args -> do
    args' <- mapM simplif args
    return $ Lprim p args'
  Lcond e1 e2 e3 -> do
    e1' <- simplif e1
    e2' <- simplif e2
    e3' <- simplif e3
    return $ Lcond e1' e2' e3'
  Lseq e1 e2 -> do
    e1' <- simplif e1
    e2' <- simplif e2
    return $ Lseq e1' e2'
  Lwhile e1 e2 -> do
    e1' <- simplif e1
    e2' <- simplif e2
    return $ Lwhile e1' e2'
  Lfor v e1 e2 e3 -> do
    e1' <- simplif e1
    e2' <- simplif e2
    e3' <- simplif e3
    return $ Lfor v e1' e2' e3'
  Lassign v e -> do 
    e' <- simplif e
    return $ Lassign v e'

eliminateArray :: Var -> [Var] -> Lambda -> Maybe Lambda
eliminateArray ary elems lam = case lam of
  Lvar v | v == ary  -> Nothing
         | otherwise -> return $ Lvar v
  Llit c -> return $ Llit c
  Lapp f args p -> do
    f' <- eliminateArray ary elems f
    args' <- mapM (eliminateArray ary elems) args
    return $ Lapp f' args' p
  Lfun params body
    | isGlobal ary -> do
      body' <- eliminateArray ary elems body
      return $ Lfun params body'
    | ary `S.member` (freeVars lam) -> Nothing
    | otherwise -> return lam
  Llet str (v, e1) e2 -> do
    e1' <- eliminateArray ary elems e1
    e2' <- eliminateArray ary elems e2
    return $ Llet str (v, e1') e2'
  Lletrec (v, e1) e2 -> do
    e1' <- eliminateArray ary elems e1
    e2' <- eliminateArray ary elems e2
    return $ Lletrec (v, e1') e2'
  Lprim ParraySet [Lvar v, Llit (IntC n), e] | v == ary
    -> do e' <- eliminateArray ary elems e
          return $ Lassign (elems!!n) e'
  Lprim ParrayRef [Lvar v, Llit (IntC n)] | v == ary
    -> return $ Lvar (elems!!n)
  Lprim p args -> do 
    args' <- mapM (eliminateArray ary elems) args
    return $ Lprim p args'
  Lcond e1 e2 e3 -> do
    e1' <- eliminateArray ary elems e1
    e2' <- eliminateArray ary elems e2
    e3' <- eliminateArray ary elems e3
    return $ Lcond e1' e2' e3'
  Lseq e1 e2 -> do
    e1' <- eliminateArray ary elems e1
    e2' <- eliminateArray ary elems e2
    return $ Lseq e1' e2'
  Lwhile e1 e2 -> do
    e1' <- eliminateArray ary elems e1
    e2' <- eliminateArray ary elems e2
    return $ Lwhile e1' e2'
  Lfor v e1 e2 e3 -> do
    e1' <- eliminateArray ary elems e1
    e2' <- eliminateArray ary elems e2
    e3' <- eliminateArray ary elems e3
    return $ Lfor v e1' e2' e3'
  Lassign v e -> do
    e' <- eliminateArray ary elems e
    return $ Lassign v e'

arrayOpt = simplif
