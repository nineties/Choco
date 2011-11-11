------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Reg (
    Reg(..), RegInfo(..), Location(..), 
    StackLocation(..), StaticLocation(..),
    emptyReg, emptyRegInfo
    ) where

import CmmSyn
import Outputable

import Control.Monad.State
import qualified Data.Map as M

data Reg = Reg {
    name      :: String,
    stamp     :: Int,
    loc       :: Location
    }
    deriving (Show)

instance Eq Reg where
    Reg{ stamp = i1 } == Reg{ stamp = i2 } = i1 == i2

instance Outputable Reg where
  ppr r = text (name r) <> char '#' <> int (stamp r) <>
    case loc r of
      Unknown -> empty
      Register n  -> brackets(char 'r' <> int n)
      Stack s -> brackets(ppr s)

data RegInfo = RegInfo {
    spill     :: Bool,
    interf    :: [Reg],
    prefer    :: [(Reg, Int)],
    degree    :: Int,
    spillCost :: Int,
    visited   :: Bool,

    location  :: Location -- for register allocation 
    }
    deriving (Show)

data Location
    = Unknown 
    | Register Int
    | Stack StackLocation
    deriving (Eq, Show, Ord)

data StackLocation
    = Local Int
    | Incoming Int
    | Outgoing Int
    deriving (Eq, Show, Ord)

instance Outputable StackLocation where
  ppr (Local n)    = text "local" <+> int n
  ppr (Incoming n) = text "in" <+> int n
  ppr (Outgoing n) = text "out" <+> int n

data StaticLocation
  = Static Int            -- global variable
  | SReg Int (Maybe Int)  -- global register (+ label of initial value)
  | HSRam Int (Maybe Int) -- high-speed ram  (+ label of initial value)
  deriving (Eq, Show, Ord)

instance Outputable StaticLocation where
  ppr (Static n)  = text "L." <> int n
  ppr (SReg n _)  = text "global register" <+> int n
  ppr (HSRam n _) = text "high speed ram" <+> int n

emptyReg :: Reg
emptyReg = Reg {
    name    = "",
    stamp   = 0,
    loc     = Unknown
    }

emptyRegInfo :: RegInfo
emptyRegInfo = RegInfo {
    spill     = False,
    interf    = [],
    prefer    = [],
    degree    = 0,
    spillCost = 0,
    visited   = False,
    location  = Unknown
    }

instance Ord Reg where
    r1 `compare` r2 = stamp r1 `compare` stamp r2
