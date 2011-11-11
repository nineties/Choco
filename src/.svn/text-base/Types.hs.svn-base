------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Types (
  TyId, Type(..), TyIdSet, TyScheme(..), 
  toScheme, freeVar, freeVarTySc,

  arrayElemSize, typeSize,

  pprTy, schemeDebugPrint,
  align, getShift
  ) where

import Id
import Outputable
import Panic

import Data.Bits
import Data.List
import qualified Data.IntSet as S
import qualified Data.IntMap as M
import Text.Printf

type TyId    = Id
type TyIdSet = S.IntSet

data Type
  = UnitT
  | BoolT
  | IntT
  | FloatT
  | FunT [Type] Type
  | TupleT [Type]
  | ArrayT Type
  | TyVar !TyId
  | UnknownT
  deriving (Eq, Show)

arrayElemSize :: Type -> Int
arrayElemSize (TupleT ts) = align $ sum $ map arrayElemSize ts
arrayElemSize _ = 1

typeSize :: Type -> Int
typeSize (TupleT ts) = sum $ map typeSize ts
typeSize _ = 1

-- move to another source file later
-- fixme!
align n = iter 1
  where
  iter x
    | n <= x  = x
    | otherwise = iter (2 * x)

getShift n = iter 1 0
  where
  iter x s
    | n <= x    = s
    | otherwise = iter (2 * x) (s + 1)


data TyScheme = TyScheme TyIdSet Type

toScheme ty = TyScheme S.empty ty

freeVar :: Type -> TyIdSet
freeVar (FunT args ret) = S.unions $ map freeVar (ret:args)
freeVar (TupleT ts)     = S.unions $ map freeVar ts
freeVar (ArrayT t)      = freeVar t
freeVar (TyVar id)      = S.singleton id
freeVar _               = S.empty

freeVarTySc :: TyScheme -> TyIdSet
freeVarTySc (TyScheme ids ty) = freeVar ty S.\\ ids

instance Outputable Type where
  ppr UnitT  = text "unit"
  ppr BoolT  = text "bool"
  ppr IntT   = text "int"
  ppr FloatT = text "float"
  ppr UnknownT = text "?"
  ppr (FunT args ret) = 
    hsep $ intersperse (text "->") (map ppr (args++[ret]))
  ppr (TupleT ts) = parens.hsep $ punctuate comma (map ppr ts)
  ppr (ArrayT e) = ppr e <> char '*'
  ppr (TyVar i) = char 'v' <> pprId i

instance Outputable TyScheme where
  ppr (TyScheme ids ty) = ppr ty

instance Show TyScheme where
  show = show.ppr

pprTy :: [(TyId, Char)] -> Type -> Doc
pprTy m t@(TyVar id) = 
  case lookup id m of
    Just c  -> char c
    Nothing -> ppr t
pprTy m (FunT args ret) =
  hsep $ intersperse (text "->") (map (pprTy m) (args ++ [ret]))
pprTy m (TupleT ts) = parens.hsep $ punctuate comma (map (pprTy m) ts)
pprTy m (ArrayT e) = pprTy m e <> char '*'
pprTy m t = ppr t

schemeDebugPrint :: TyScheme -> IO ()
schemeDebugPrint (TyScheme ids ty)
  = do printf "forall %s -> %s\n" (show $ S.toList ids) (show ty)
