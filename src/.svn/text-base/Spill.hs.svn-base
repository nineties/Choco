------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Spill where

import Choco
import Mach
import Outputable
import Proc
import Reg
import RegM

{- Insertion of moves to suggest possible spilling / reloading points
   before register allocation -}

import Control.Monad.State
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

type P a = StateT Env ChocoM a
type Subst = M.Map Reg Reg

data Env = Env {
  spill_env :: Subst, -- Association of spill registers to registers
  use_date  :: M.Map Reg Int, -- Record the position of last use of registers
  current_date :: Int,

  -- Destroyed at if-then-else points
  destroyed_at_fork :: [(Inst, S.Set Reg)]
  }

initEnv = Env {
  spill_env = M.empty,
  use_date  = M.empty,
  current_date = 0,
  destroyed_at_fork = []
  }

incrCurrentDate :: Int -> P ()
incrCurrentDate n = modify $ \e -> e{ current_date = (current_date e) + n }

spillReg :: Reg -> P Reg
spillReg r = do
  env <- gets spill_env
  case M.lookup r env of
    Just r'  -> return r'
    Nothing -> do
      spill_r <- lift $ createReg 
      lift $ modifyRegInfo spill_r $ \i -> i{ spill = True }
      let spill_r' = spill_r{
        name = if name r /= ""
          then "spilled-" ++ name r
          else ""
        }
      modify $ \e -> e{ spill_env = M.insert r spill_r' (spill_env e) }
      return spill_r'

recordUse :: [Reg] -> P ()
recordUse =
  mapM_ (\r -> do
    e@Env{ use_date = u, current_date = c } <- get
    let prev_date = M.findWithDefault 0 r u
    when (c > prev_date) $
      put e{ use_date = M.insert r c u }
    )


-- Check if the register pressure overflows the maximum pressure allowed
-- at that point. If so, spill enough registers to lower the pressure.
addSuperPressureRegs op live_regs res_regs spilled = do
  let max_pressure = maxRegisterPressure op
  let regs = foldr S.insert live_regs res_regs
  -- Compute the pressure
  pressure <- foldM (\p r -> 
    if S.member r spilled
      then return p
      else case loc r of
        Stack _ -> return p
        _ -> return (p+1)
    ) 0 (S.toList regs)
  -- Check if pressure is exceeded
  if pressure <= max_pressure
    then return spilled
    else do
      -- Find the least recently used, unspilled, unallocated, live register
      (lru_date, lru_reg) <- foldM (\(lrud, lrur) r ->
        if S.notMember r spilled && loc r == Unknown
          then do
            ud <- gets use_date
            case M.lookup r ud of
              Just d -> if d < lrud
                then return (d, r)
                else return (lrud, lrur)
              Nothing -> return (lrud, lrur)
          else return (lrud, lrur)
        ) (1000000, emptyReg) (S.toList live_regs)
      if lru_reg /= emptyReg
        then return $ S.insert lru_reg spilled
        else return spilled


-- First pass: insert reload instructions based on an approximation of
-- what is destroyed at pressure points
addReloads regset i =
  foldM (\i r -> do
    spill_r <- spillReg r
    return $ consInst (Iop Ireload) [r] [spill_r] i
    ) i (S.toList regset)


reload i before = do
  incrCurrentDate 1
  recordUse (args i)
  recordUse (result i)
  case idesc i of
    Iend  -> return (i, before)

    Ireturn -> do
      i' <- addReloads (before `S.intersection` (S.fromList$args i)) i
      return (i', S.empty)

    Iop Itailcall_ind -> do
      i' <- addReloads (before `S.intersection` (S.fromList$args i)) i
      return (i', S.empty)

    Iop (Itailcall_imm _) -> do
      i' <- addReloads (before `S.intersection` (S.fromList$args i)) i
      return (i', S.empty)

    Iop Icall_ind -> do
      (new_next, finally) <- reload (next i) (live i)
      i' <- addReloads (before `S.intersection` (S.fromList$args i))
        (consInst (Iop Icall_ind) (result i) (args i) new_next)
      return (i', finally)

    Iop (Icall_imm s) -> do
      (new_next, finally) <- reload (next i) (live i)
      i' <- addReloads (before `S.intersection` (S.fromList$args i))
        (consInst (Iop (Icall_imm s)) (result i) (args i) new_next)
      return (i', finally)

    Iop op -> do
      new_before <- 
        if S.size (live i) + length (result i) <= safeRegisterPressure op
          then return before
          else addSuperPressureRegs op (live i) (result i) before
      let after = new_before S.\\ (S.fromList (args i)) 
                             S.\\ (S.fromList (result i))

      (new_next, finally) <- reload (next i) after
      i' <- addReloads (new_before `S.intersection` (S.fromList$args i))
          (consInst (Iop op) (result i) (args i) new_next)
      return (i', finally)

    Icond tst ifso ifnot -> do
      let at_fork = before S.\\ (S.fromList (args i))
      date_fork <- gets current_date
      (new_ifso, after_ifso) <- reload ifso at_fork
      date_ifso <- gets current_date
      modify $ \e -> e{ current_date = date_fork }

      (new_ifnot, after_ifnot) <- reload ifnot at_fork
      modify $ \e -> e{ current_date = max date_ifso (current_date e) }

      (new_next, finally) <-
        reload (next i) (after_ifso `S.union` after_ifnot)

      let new_i = consInst (Icond tst new_ifso new_ifnot) 
                      (result i) (args i) new_next

      modify $ \e -> e{
        destroyed_at_fork = (new_i, at_fork) : (destroyed_at_fork e)
        }

      i' <- addReloads (before `S.intersection` (S.fromList$args i)) new_i
      return (i', finally)


-- Second pass: add spill instructions based on what we've decided to reload.
-- That is, any register that may be reloaded in the future must be spilled
-- just after its definition
addSpills regset i =
  foldM (\i r -> do
    spill_r <- spillReg r
    return $ consInst (Iop Ispill) [spill_r] [r] i
    ) i (S.toList regset)

doSpill :: Inst -> S.Set Reg -> P (Inst, S.Set Reg)
doSpill i finally =
  case idesc i of
    Iend -> return (i, finally)

    Ireturn -> return (i, S.empty)
    Iop Itailcall_ind -> return (i, S.empty)
    Iop (Itailcall_imm _) -> return (i, S.empty)

    Iop Ireload -> do
      (new_next, after) <- doSpill (next i) finally
      let before' = after S.\\ (S.fromList $ result i)
      return 
        (consInst (Iop Ireload) (result i) (args i) new_next,
         S.union before' (S.fromList$result i))
    
    Iop _ -> do
      (new_next, after) <- doSpill (next i) finally
      let before = after S.\\ (S.fromList $ result i)
      next' <- addSpills (after `S.intersection` (S.fromList $ result i)) new_next
      return (consInst (idesc i) (result i) (args i) next', before)

    Icond tst ifso ifnot -> do
      (new_next, at_join)       <- doSpill (next i) finally
      (new_ifso, before_ifso)   <- doSpill ifso at_join
      (new_ifnot, before_ifnot) <- doSpill ifnot at_join

      tab <- gets destroyed_at_fork
      Just destroyed <- return . lookup i =<< gets destroyed_at_fork

      let spill_ifso_branch = before_ifso S.\\ before_ifnot S.\\ destroyed
      let spill_ifnot_branch = before_ifnot S.\\ before_ifso S.\\ destroyed
      
      ifso'  <- addSpills spill_ifso_branch new_ifso
      ifnot' <- addSpills spill_ifnot_branch new_ifnot
      return 
        (consInst (Icond tst ifso' ifnot') (result i) (args i) new_next,
        (before_ifso `S.union` before_ifnot) 
          S.\\ spill_ifso_branch S.\\ spill_ifnot_branch)


-- Entry point
fundecl :: FunDec -> ChocoM FunDec
fundecl f = evalStateT (do
  (body1, _) <- reload (fun_body f) S.empty
  (body2, tospill_at_entry) <- doSpill body1 S.empty
  new_body <- addSpills 
    (tospill_at_entry `S.intersection` S.fromList (fun_args f)) body2
  return f{ fun_body = new_body }
  ) initEnv
