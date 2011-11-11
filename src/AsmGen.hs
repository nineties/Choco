------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module AsmGen (
    asmgen
    ) where

import Choco
import Coloring
import Comballoc
import Interf
import Selection
import Linearize
import Liveness
import LocalCSE
import Mach
import Schedular
import Outputable
import Proc
import Reload
import Reg
import Schedular
import Spill
import Split

import Control.Monad.State
import qualified Data.Map as M

liveness fun = Liveness.fundecl fun

asmgen flags itab ftab fun = do
  resetRegs
  fun1 <- Selection.fundecl fun itab ftab
  when (dump_selection flags) $
     report "After instruction selection" (ppr fun1)

  fun2 <- LocalCSE.fundecl fun1
  when (dump_lcse flags) $
    report "After local common subexpression elimination" (ppr fun2)

  fun3 <- Comballoc.fundecl fun2
  when (dump_combine flags) $
   report "After allocation combining" (ppr fun3)

  fun4 <- liveness fun3
  when (dump_live flags) $
    report "Liveness analysis" (ppr fun4)

  fun5 <- Spill.fundecl fun4
  when (dump_spill flags) $
    report "After spilling" (ppr fun5)

  fun6 <- Split.fundecl =<< liveness fun5
  when (dump_split flags) $
    report "After live range splitting" (ppr fun6)

  (fun7, nslots) <- regalloc flags 1 =<< liveness fun6

  fun8 <- Linearize.fundecl fun7
  when (dump_linear flags) $
    report "Linearized code" (ppr fun8)

  fun9 <- Schedular.fundecl fun8
  when (dump_scheduling flags) $
    report "After instruction scheduling" (ppr fun9)

  return (fun9, nslots)

regalloc flags round fd 
  | round > 50 
    = simpleError $ text "cannot complete register allocation:"<+> ppr fd
  | otherwise
    = do

    when (dump_live flags) $
      report "Liveness analysis (during register allocation)" (ppr fd)

    Interf.buildGraph fd
    (fd', nslots) <- Coloring.allocateRegisters fd

    when (dump_regalloc flags) $
      report "After register allocation" (ppr fd')

    (fd'', redo_regalloc) <- Reload.fundecl fd'
    when (dump_reload flags) $
      report "After insertion of reloading code" (ppr fd'')

    if redo_regalloc
      then do
        reinitRegs
        fd''' <- liveness fd''
        regalloc flags (round + 1) fd'''
      else return (fd'', nslots)

