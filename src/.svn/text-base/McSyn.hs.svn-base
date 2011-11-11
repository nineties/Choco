------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module McSyn (
    LMcStmt, LMcPat, LMcExpr,
    McStmt(..), McPat(..), McExpr(..), PreOp(..), BinOp(..)
    ) where

import Common
import Const
import SrcLoc
import Panic
import Primitive
import Outputable
import Var

import Data.List

{- abstract syntax of MinCaml -}

type LMcStmt = Located McStmt
type LMcPat  = Located McPat
type LMcExpr = Located McExpr

data McStmt
    = McEvalS LMcExpr
    | McValueS (LMcPat, LMcExpr)
    deriving (Eq)

data McExpr
    = McVarE Name
    | McLitE Const
    | McAppE LMcExpr [LMcExpr]
    | McPrefixE PreOp LMcExpr
    | McInfixE BinOp LMcExpr LMcExpr
    | McLetE RecFlag (LMcPat, LMcExpr) LMcExpr
    | McFunE [LMcPat] LMcExpr
    | McSeqE LMcExpr LMcExpr
    | McCondE LMcExpr LMcExpr LMcExpr
    | McTupleE [LMcExpr]
    deriving (Eq)


data McPat
    = McAnyP
    | McVarP Name
    | McTupleP [LMcPat]
    deriving (Eq)

instance Outputable McStmt where
  ppr = pprStmt
instance Outputable McExpr where
  ppr = pprExpr
instance Outputable McPat where
  ppr = pprPat

pprStmt (McEvalS expr) = ppr expr
pprStmt (McValueS (var, val))
  = hang (text "let") 2 (ppr var <+> char '=' <+> ppr val)

pprExpr (McVarE id) = ppr id
pprExpr (McLitE c) = ppr c
pprExpr (McAppE f args) 
  = hang (ppr f) 2 (sep (map pprParendExpr args))
pprExpr (McPrefixE op e)
  = ppr op <+> pprParendExpr e
pprExpr (McInfixE op e1 e2)
  = pprParendExpr e1 <+> ppr op <+> pprParendExpr e2
pprExpr (McLetE rec (var, val) expr)
  = let letstr = case rec of
          Rec -> text "let rec"; NonRec -> text "let"
    in sep [hang letstr 2 (ppr var <+> char '=' $$ ppr val),
        text "in",
        ppr expr]
pprExpr (McFunE args body)
  = sep [hsep (text "fun" : intersperse (text "->") (map ppr args)) <+> text "->", 
         nest 2 (ppr body)
        ]
pprExpr (McSeqE e1 e2)
  = ppr e1 <> semi $$ ppr e2
pprExpr (McCondE e1 e2 e3)
  = sep [hsep [text "if", nest 2 (ppr e1)],
         nest 2 (text "then" <+> ppr e2),
         nest 2 (text "else" <+> ppr e3)
         ]
pprExpr (McTupleE elems) = parens.sep $ punctuate comma (map ppr elems)

pprParendExpr expr
    = let pp = ppr expr
      in case unLoc expr of
              McVarE id -> ppr id
              McLitE c -> ppr c
              _ -> parens pp

pprPat McAnyP = char '_'
pprPat (McVarP id) = ppr id
pprPat (McTupleP elems) = parens.sep $ punctuate comma (map ppr elems)
