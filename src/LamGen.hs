------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module LamGen (
  translLambda
  ) where

import Choco
import Common
import Const
import LamSyn
import Panic
import Primitive
import Outputable
import SrcLoc
import TcSyn
import Types
import Var

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

{- Lambda transformation Monad -}
type LamM a = StateT () ChocoM a

{- translate typed-expression to typed-lambda-language -}
translStmts :: [LTcStmt] -> LamM Lambda
translStmts [L _ (TcEvalS e)] = translExpr e
translStmts [L _ (TcValueS _)] = return $ Llit UnitC
translStmts (L _ (TcEvalS e) : rem) 
  = do e'   <- translExpr e
       rem' <- translStmts rem
       return $ Lseq e' rem'
translStmts (L _ (TcValueS bind) : rem) 
  = do rem' <- translStmts rem
       translLet bind rem'


translExpr :: TcExpr -> LamM Lambda
translExpr e
  = case expr_desc e of
  TcVarE v -> return $ Lvar v
  TcLitE c  -> return $ Llit c
  TcAppE f args 
    -> do translApply f args

  TcPrefixE op e
    -> do e' <- translExpr e
          return $ (Lprim (translPreOp op (expr_type e)) [e']) 

  TcInfixE op e1 e2
    -> do e1' <- translExpr e1
          e2' <- translExpr e2
          return $ (Lprim (translBinOp op (expr_type e2)) [e1', e2'])

  TcLetE rec bind body
    -> do body' <- translExpr body
          translLet bind body'

  TcFunE args body
    -> translFunction args body

  TcTupleE elems
    -> do elems' <- mapM translExpr elems
          return $ (Lprim PcreateTuple elems')

  TcCondE e1 e2 e3
    -> do e1' <- translExpr e1
          e2' <- translExpr e2
          e3' <- translExpr e3
          return $ Lcond e1' e2' e3'

  TcSeqE e1 e2
    -> do e1' <- translExpr e1
          e2' <- translExpr e2
          return $ Lseq e1' e2'

translApply :: TcExpr -> [TcExpr] -> LamM Lambda
translApply f args
  = do let FunT _ ret = expr_type f
       let retp = if ret /= UnitT then True else False
       f' <- translExpr f
       args' <- mapM translExpr args
       case f' of
        (Lapp f2 args2 p) -> return $ Lapp f2 (args2 ++ args') retp
        (Lvar v) | isGlobal v ->
          case M.lookup (var_name v) (snd primTable) of
            Just fn -> return (fn args')
            Nothing -> return $ Lapp f' args' retp
        _ -> return $ Lapp f' args' retp

isSimpleLambda :: Lambda -> Bool
isSimpleLambda lam
  = case lam of
    Lvar _  -> True  
    Llit (FloatC _) -> False
    Llit _ -> True
    _ -> False

translLet :: (TcPat, TcExpr) -> Lambda -> LamM Lambda
translLet (pat, expr) cont
  = do expr' <- translExpr expr
       case pat_desc pat of
         TcAnyP -> return $ Lseq expr' cont
         TcVarP v -> if not $ checkRecursive [v] expr'
          then return $ Lletrec (v, expr') cont
          else return $ Llet Strict (v, expr') cont
         TcTupleP pats 
          -> if isSimpleLambda expr' 
            then return =<< walk pats 0 expr' cont
            else do v <- lift $ mkTmpVar "match" (toScheme $ expr_type expr)
                    lam <- walk pats 0 (Lvar v) cont
                    return $ Llet Strict (v, expr') lam
    where
    walk [] _ expr cont = return cont
    walk (TcPat TcAnyP _ _ : rem) i expr cont
      = walk rem (i+1) expr cont
    walk (TcPat (TcVarP v) _ _ : rem) i expr cont
      = walk rem (i+1) expr
            (Llet Strict (v, (Lprim (PtupleRef i) [expr])) cont)
    walk (TcPat (TcTupleP pats) _ _ : rem) i expr cont
      = do cont' <- walk pats 0 (Lprim (PtupleRef i) [expr]) cont
           walk rem (i+1) expr cont'

translFunction :: [TcPat] -> TcExpr -> LamM Lambda
translFunction pats body = do
  body' <- translExpr body
  walk pats [] body'
  where
  walk [] args body  = return $ Lfun (reverse args) body
  walk (TcPat TcAnyP _ ty : rem) args body
    = do v <- lift $ mkTmpVar "a" (toScheme ty)
         walk rem (v : args) body
  walk (TcPat (TcVarP v) _ ty : rem) args body
    = walk rem (v : args) body
  walk (p@(TcPat (TcTupleP pats) _ ty) : rem) args body
    = do v <- lift $ mkTmpVar "t" (toScheme ty)
         body' <- translLet (p, TcExpr (TcVarE v) noSrcLoc ty) body
         walk rem (v : args) body'

translPreOp :: PreOp -> Type -> Prim
translPreOp op _
  = case op of
    Neg  -> Pnegi
    FNeg -> Pnegf

translBinOp :: BinOp -> Type -> Prim
translBinOp op t
  = case op of
    Add  -> Paddi
    Sub  -> Psubi
    Mul  -> Pmuli
    Div  -> Pdivi
    FAdd -> Paddf
    FSub -> Psubf
    FMul -> Pmulf
    FDiv -> Pdivf
    cmp  -> (cmptype t) (transl op)
  where
  cmptype IntT   = Pcompi
  cmptype FloatT = Pcompf
  cmptype _ = Pcompi
  -- cmptype _      = panic $ "invalid argument type for comparison : " ++ show t

  transl Eq = Ceq
  transl Ne = Cne
  transl Le = Cle
  transl Ge = Cge
  transl Lt = Clt
  transl Gt = Cgt
  trans _   = panic $ "invalid operator for comparison : " ++ show op


checkRecursive :: [Var] -> Lambda -> Bool
checkRecursive = check_top
  where
  check_top idlist lam
    = case lam of
      Lvar v -> notElem v idlist
      Llet _ (v, e) cont -> 
        check idlist e && check_top (add_let v e idlist) cont
      Lseq l1 l2 -> check idlist l1 && check_top idlist l2
      lam -> check idlist lam

  check idlist lam
    = case lam of
      Lvar _ -> True
      Llet _ (v, e) cont ->
        check idlist e && check (add_let v e idlist) cont
      Lletrec (v, e) cont ->
        check idlist e && check (add_let v e idlist) cont
      Lseq e1 e2 -> check idlist e1 && check idlist e2
      lam -> 
        let fv = freeVars lam in
        all (`S.notMember` fv) idlist

  add_let id e idlist =
    let fv = freeVars e in
    if any (`S.member` fv) idlist
      then id : idlist
      else idlist

{- Interfaces -}
translLambda :: [LTcStmt] -> ChocoM Lambda
translLambda stmts = evalStateT (translStmts stmts) ()
