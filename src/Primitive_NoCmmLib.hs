------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Primitive (
  PreOp(..), BinOp(..),
  preopType, binopType,
  arrayConName, arraySetName, arrayGetName,
  primTable, extPrefix
  ) where

import Const
import LamSyn
import qualified Outputable as O
import Panic
import Types
import Var

import qualified Data.Map as M
import qualified Data.IntSet as S

data PreOp = Neg | FNeg deriving (Eq, Show)
data BinOp 
  = Add | Sub | Mul | Div | FAdd | FSub | FMul | FDiv
  | Eq | Ne | Le | Ge | Lt | Gt
  deriving (Eq, Show, Ord)

instance O.Outputable PreOp where
  ppr Neg = O.char '-'
  ppr FNeg = O.text "-."
instance O.Outputable BinOp where
  ppr Add = O.char '+'
  ppr Sub = O.char '-'
  ppr Mul = O.char '*'
  ppr Div = O.char '/'
  ppr FAdd = O.text "+."
  ppr FSub = O.text "-."
  ppr FMul = O.text "*."
  ppr FDiv = O.text "/."
  ppr Eq = O.char '='
  ppr Ne = O.text "<>"
  ppr Le = O.text "<="
  ppr Ge = O.text ">="
  ppr Lt = O.char '<'
  ppr Gt = O.char '>'

preopType Neg  = IntT
preopType FNeg = FloatT

binopType op 
  | Add  <= op && op <= Div  = Just IntT
  | FAdd <= op && op <= FDiv = Just FloatT
  | otherwise                = Nothing


arraySetName = "set_array"
arrayGetName = "get_array"
arrayConName = "create_array"

{- primitive functions -}
extPrefix = "lib_"
mkExtCall name b r = Pextcall (extPrefix ++ name) b r

primTable :: (VarMap, LamMap)
primTable = 
  (\(vm, lm) -> (M.fromList vm, M.fromList lm)) $
  unzip $ 
  map (\(id,name,ty,fn) -> ((mkName name, (ty,id,True)), (name,fn))) $
  [
  (0, "not",
    toScheme $ bool --> bool,
    Lprim Pnot
  ),

  (1, "create_array", 
    forall [a'] (int --> a' --> array a'),
    Lprim PcreateArray
  ),

  (2, "get_array", 
    forall [a'] (array a' --> int --> a'),
    Lprim ParrayRef
  ),

  (3, "set_array",
    forall [a'] (array a' --> int --> a' --> unit),
    Lprim ParraySet
  ),

  (4, "int_of_float",
    toScheme $ float --> int,
    Lprim (mkExtCall "int_of_float" False True)
  ),

  (5, "float_of_int",
    toScheme $ int --> float,
    Lprim (mkExtCall "float_of_int" False True)
  ),

  (6, "print_byte", 
    toScheme $ int --> unit,
    Lprim Pput
  ),

  (7, "print_char",
    toScheme $ int --> unit,
    Lprim Pput
  ),

  (8, "print_int", 
    toScheme $ int --> unit,
    Lprim (mkExtCall "print_int" True False)
  ),

  (9, "read_int", 
    toScheme $ unit --> int,
    Lprim (mkExtCall "read" True True)
  ),

  (10, "read_float", 
    toScheme $ unit --> float,
    Lprim (mkExtCall "read" True True)
  ),

  (11, "sin", 
    toScheme $ float --> float,
    Lprim (mkExtCall "sin" False True)
  ),
  (12, "cos", 
    toScheme $ float --> float,
    Lprim (mkExtCall "cos" False True)
  ),
  (13, "atan", 
    toScheme $ float --> float,
    Lprim (mkExtCall "atan" False True)
  ),

  (14, "sqrt", 
    toScheme $ float --> float,
    Lprim Psqrt
  ),

  (15, "floor", 
    toScheme $ float --> float,
    Lprim (mkExtCall "floor" False True)
  ),

  (16, "fiszero", 
    toScheme $ float --> bool,
    \[arg] -> Lprim (Pcompf Ceq) [arg, Llit (FloatC 0.0)]
  ),

  (17, "fispos",  
    toScheme $ float --> bool,
    \[arg] -> Lprim (Pcompf Cgt) [arg, Llit (FloatC 0.0)]
  ),

  (18, "fisneg",  
    toScheme $ float --> bool,
    \[arg] -> Lprim (Pcompf Clt) [arg, Llit (FloatC 0.0)]
  ),

  (19, "fless",   
    toScheme $ float --> float --> bool,
    Lprim (Pcompf Clt)
  ),

  (20, "fabs", 
    toScheme $ float --> float,
    Lprim Pabsf
  ),

  (30, "fneg", 
    toScheme $ float --> float,
    Lprim Pnegf
  ),

  (40, "fhalf",
    toScheme $ float --> float,
    \[arg] -> Lprim Pdivf [arg, Llit (FloatC 2.0)]
  ),

  (41, "fsqr", 
    toScheme $ float --> float,
    \[arg] -> Lprim Pmulf [arg, arg]
  )
  ]

{- utilities -}
a'      = TyVar 0
unit    = UnitT
bool    = BoolT
int     = IntT
float   = FloatT
array x = ArrayT x

forall ts ty 
  = let ids = S.fromList $ map untag ts
    in TyScheme ids ty
  where
  untag (TyVar id) = id
  untag _ = panic "Primitive.forall"

infixr 1 -->
(-->) :: Type -> Type -> Type
a --> FunT args ret = FunT (a:args) ret
a --> b = FunT [a] b

