------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module TcSyn (
  TcPat(..), TcPatDesc(..),
  TcExpr(..), TcExprDesc(..),
  TcStmt(..), LTcStmt
  ) where

import Common
import Const
import McSyn
import Outputable
import Panic
import SrcLoc
import Types
import Var

import Data.List
import qualified Data.IntSet as S

{- Abstract syntax after typing -}

data TcPat = TcPat {
  pat_desc  :: TcPatDesc,
  pat_loc   :: SrcLoc,
  pat_type  :: Type
  }
  deriving (Eq)

data TcPatDesc
  = TcAnyP
  | TcVarP Var
  | TcTupleP [TcPat]
  deriving (Eq)

data TcExpr = TcExpr {
  expr_desc :: TcExprDesc,
  expr_loc  :: SrcLoc,
  expr_type :: Type
  }
  deriving (Eq)

data TcExprDesc
  = TcVarE Var
  | TcLitE Const
  | TcAppE TcExpr [TcExpr]
  | TcPrefixE PreOp TcExpr
  | TcInfixE BinOp TcExpr TcExpr
  | TcLetE RecFlag (TcPat, TcExpr) TcExpr
  | TcFunE [TcPat] TcExpr
  | TcTupleE [TcExpr]
  | TcCondE TcExpr TcExpr TcExpr
  | TcSeqE TcExpr TcExpr
  deriving (Eq)

data TcStmt
  = TcEvalS TcExpr
  | TcValueS (TcPat, TcExpr)
  deriving (Eq)

type LTcStmt = Located TcStmt

instance Show TcPat where
  show = show.ppr
instance Outputable TcPat where
  ppr = pprPat []

instance Show TcExpr where
  show = show.ppr
instance Outputable TcExpr where
  ppr = pprExpr []

instance Show TcStmt where
  show = show.ppr
instance Outputable TcStmt where
  ppr stmt = pprStmt stmt

pprVar m v@Var{ var_type = TyScheme _ ty } 
  = ppr v <> brackets (pprTy m ty)

pprPat _ TcPat{ pat_desc = TcAnyP } = char '_'
pprPat m TcPat{ pat_desc = TcVarP v } = pprVar m v
pprPat m TcPat{ pat_desc = TcTupleP elems }
  = parens.sep $ punctuate comma (map (pprPat m) elems)

pprExpr m TcExpr{ expr_desc = TcVarE v } = pprVar m v
pprExpr _ TcExpr{ expr_desc = TcLitE c } = ppr c
pprExpr m TcExpr{ expr_desc = TcAppE f args }
  = hang (pprExpr m f) 2 (sep (map (pprParendExpr m) args))
pprExpr m TcExpr{ expr_desc = TcPrefixE op e }
  = ppr op <+> pprParendExpr m e
pprExpr m TcExpr{ expr_desc = TcInfixE op e1 e2 }
  = pprParendExpr m e1 <+> ppr op <+> pprParendExpr m e2
pprExpr m TcExpr{ expr_desc = TcLetE rec bind e }
  = let letstr = case rec of
          Rec -> text "let rec"; NonRec -> text "let"
    in sep [hang letstr 2 (pprBind bind),
            text "in",
            pprExpr m e]
pprExpr m TcExpr{ expr_desc = TcFunE args body }
  = sep [hsep (text "fun" : 
      intersperse (text "->") (map (pprPat m) args)) <+> text "->",
         nest 2 (pprExpr m body)]
pprExpr m TcExpr{ expr_desc = TcTupleE elems }
  = parens.sep $ punctuate comma (map (pprExpr m) elems)
pprExpr m TcExpr{ expr_desc = TcCondE e1 e2 e3 }
  = sep [hsep [text "if", nest 2 (pprExpr m e1)],
         nest 2 (text "then" <+> pprExpr m e2),
         nest 2 (text "else" <+> pprExpr m e3)
         ]
pprExpr m TcExpr{ expr_desc = TcSeqE e1 e2 }
  = pprExpr m e1 <> semi $$ pprExpr m e2

pprBind (pat, expr) 
  = let m =  [] -- zip (S.toList $ grepIds (pat_desc pat)) ['a'..]
    in pprPat m pat <+> char '=' <+> pprExpr m expr
  where
  grepIds TcAnyP = S.empty
  grepIds (TcVarP (Var{ var_type = TyScheme ids _ })) = ids
  grepIds (TcTupleP pats) = S.unions $ map (grepIds . pat_desc) pats

pprParendExpr m expr
  = let pp = pprExpr m expr
     in case expr_desc expr of
          TcVarE _ -> pp
          TcLitE _ -> pp
          _ -> parens pp

pprStmt (TcEvalS e)     = ppr e
pprStmt (TcValueS bind) = text "let" <+> pprBind bind
