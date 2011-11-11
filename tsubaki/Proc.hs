------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Proc where

import Choco
import Mach
import Reg
import RegM

import Control.Monad.State
import qualified Data.Map as M

boolRepr True  = 1
boolRepr False = -1

destroyedAtOper :: InstDesc -> ChocoM [Reg]
destroyedAtOper desc = case desc of
  Iop Icall_ind     -> gets phys_regs
  Iop (Icall_imm s) -> do
    table <- gets fun_reg_info
    case M.lookup s table of
      Just regs -> return regs
      Nothing   -> gets phys_regs
  _                 -> return []
  

maxRegisterPressure :: Operation -> Int
maxRegisterPressure op = 24

safeRegisterPressure :: Operation -> Int
safeRegisterPressure op = 12

hsramBegin, hsramEnd, numHSRAM :: Int 
hsramBegin = 60
hsramEnd = 256
numHSRAM = hsramEnd - hsramBegin

normalRegFirst = 2
normalRegEnd   = 30
numNormalReg   = normalRegEnd - normalRegFirst

globalRegBegin, globalRegEnd, numGlobalReg :: Int
globalRegBegin = normalRegEnd
globalRegEnd   = 60 
numGlobalReg   = globalRegEnd - globalRegBegin

globalRegs :: ChocoM [Reg]
globalRegs = mapM physReg [globalRegBegin .. globalRegEnd - 1]

isGlobalReg r = case loc r of 
  Register n -> globalRegBegin <= n && n < globalRegEnd
  _ -> False

numPhysicalReg :: Int
numPhysicalReg = 64

physReg n = do 
  regs <- gets phys_regs 
  return $ regs !! n


stackSlot slot = do
  r <- createReg
  return $ r{ name = "S", loc = Stack slot }

callingConventions first last make_stack args
  = do
  iter first 0 args []
  where
  iter _ ofs [] ret = return (reverse ret, ofs)
  iter i s (a:as) ret 
    = if i <= last
        then do
          r <- physReg i
          iter (i+1) s as (r : ret)
        else do
          r <- stackSlot (make_stack s)
          iter i (s+1) as (r : ret)
        
locArguments args  
  = callingConventions 3 (normalRegEnd-1) Outgoing args

locParameters args 
  = return.fst =<< callingConventions 3 (normalRegEnd-1) Incoming args

locResults res     
  = return.fst =<< callingConventions 2 2 undefined res

resetRegs :: ChocoM ()
resetRegs = do
  modify $ \e -> e{ reg_stamp = 0 }

  regs <- mapM (\i -> do 
    r <- createReg
    modifyRegInfo r $ \info -> info{ location = Register i }
    return r{ name = "R", loc = Register i }
    ) [0 .. numPhysicalReg-1]

  modify $ \e -> e{ 
    phys_regs = regs,
    reg_list = [] 
    }

  setPrimFunInfo

reinitRegs :: ChocoM ()
reinitRegs = do
  reg_list' <- mapM reinit =<< gets reg_list
  modify $ \e -> e{ reg_list = reg_list' }

  where
  reinit r = do
    modifyRegInfo r $ \i -> 
      i{ location = Unknown, 
         interf = [], 
         prefer = [], 
         degree = 0,
         spillCost = if spillCost i >= 100000 then 100000 else 0
       }
    return r{ loc = Unknown }
{- utilities -}
-- move another source file later
isSignedImm6    n  = n <= 31    && n >= -32
isUnsignedImm8 n  = n <= 255   && n >= 0
isUnsignedImm10 n = n <= 1023 && n >= 0
isUnsignedImm14 n = n <= 16383 && n >= 0

updateRegisterLocation fd
  = do
  m <- gets reg_info_table
  let args' = map (set m) (fun_args fd)
  i' <- iter m (fun_body fd)
  return fd{ fun_args = args', fun_body = i' }

  where
  iter m i | idesc i == Iend = return i
  iter m i = do
    let result' = map (set m) (result i)
    let args'   = map (set m) (args i)
    idesc' <- case idesc i of
      Icond test i1 i2 -> do
        i1' <- iter m i1
        i2' <- iter m i2
        return $ Icond test i1' i2'
      _ -> return (idesc i)

    next' <- iter m (next i)
    return i{ result = result', args = args', idesc = idesc', next = next' }

  set table reg = reg{ loc = location $ table M.! reg }

-- move this table another place
setPrimFunInfo :: ChocoM ()
setPrimFunInfo = do
  mapM_ (\(name, nums) -> do
    regs <- mapM physReg nums
    modify $ \e -> e{ fun_reg_info = M.insert name regs (fun_reg_info e) }
    ) [("lib_read", [2,3,4]),
       ("lib_create_array", [2,3,4,5]),
       ("lib_int_of_float", [2,3,4,5,6,7]),
       ("lib_float_of_int", [2,3,4,5,6]),
       ("lib_floor", [2,3,4,5,6,7]),
       ("lib_sin", [2,3,4,5,6,7]),
       ("lib_cos", [2,3,4,5,6,7]),
       ("lib_atan",[2,3,4,5,6,7,8,9,10])]
