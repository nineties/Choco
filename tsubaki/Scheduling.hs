------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Scheduling (
    opLatency,
    opIssueCycles,
    opInBasicBlock
    ) where

import Mach hiding (Inst(..))

import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Control.Monad.State as ST
import Control.Monad

{- instruction informations -}
opLatency op = case op of
  Ireload -> 2
  Iload _ -> 2
  Istore _  -> 2
  Iconst_float _ -> 2
  Iput -> 2
  Iget -> 2
  _ | op `elem` [Isqrt, Ifinv, Ifadd, Ifsub, Ifmul]
    -> 3
  _ -> 1

opIssueCycles op = case op of
  Iconst_float _ -> 2
  Iconst_symbol _ -> 2
  _ -> 1

opInBasicBlock op = case op of
    Icall_ind   -> False
    Icall_imm _ -> False
    Itailcall_ind   -> False
    Itailcall_imm _ -> False
    _   -> True
