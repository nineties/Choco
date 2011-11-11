------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Const (
  Const(..),
  nullPtr
  ) where

import Outputable

{- constant values -}

data Const
    = IntC Int
    | FloatC Float
    | UnitC
    | BoolC Bool
    | PointerC Int
    deriving (Eq)

nullPtr = PointerC 0

instance Outputable Const where
    ppr (IntC i)      = int i
    ppr (FloatC f)    = float f
    ppr UnitC         = text "()"
    ppr (BoolC True)  = text "true"
    ppr (BoolC False) = text "false"
    ppr (PointerC 0)  = text "null"
    ppr (PointerC n)  = text "p" <> int n

instance Show Const where
    show = show.ppr
