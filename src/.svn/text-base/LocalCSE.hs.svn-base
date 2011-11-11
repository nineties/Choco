------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module LocalCSE where

import Choco
import Mach
import Reg

import Control.Monad.State
import qualified Data.Map as M

{- Common Subexpression Elimination -}

type Index = Int

data Node 
  = Node {
    operation :: Operation,
    operands  :: [Index]
    }
  | Leaf Index
  deriving (Show, Eq, Ord)

type P a = StateT Env ChocoM a

data Env = Env {
  vntable   :: M.Map Reg Index,
  adaq      :: M.Map Node Index
  }

initEnv = Env {
  vntable = M.empty,
  adaq    = M.empty
  }

reset = put initEnv

cse :: Inst -> P Inst
cse i = case idesc i of
  Iend      -> return i
  Ireturn   -> return i

  Iop Icall_ind -> do
    reset
    next' <- cse (next i)
    return i{ next = next' }

  Iop (Icall_imm s) -> do
    reset
    next' <- cse (next i)
    return i{ next = next' }

  Iop Itailcall_ind -> do
    reset
    next' <- cse (next i)
    return i{ next = next' }

  Iop (Itailcall_imm s) -> do
    reset
    next' <- cse (next i)
    return i{ next = next' }

  Iop Imove
     -> do 
      regNo (args i!!0) >>= setRegNo (result i!!0) 
      next' <- cse (next i)
      return i{ next = next' }

  Iop (Iintop op)       -> cseExp i
  Iop (Iintop_imm op n) -> cseExp i
  Iop op | op `elem` [Ifabs, Ifneg, Isqrt, Ifinv, Ifadd, Ifsub, Ifmul, Iftoi, Iitof]
    -> cseExp i
  Iop (Ihsr _) -> cseExp i

  Iop _ -> do
    next' <- cse (next i)
    return i{ next = next' }

  Icond tst i1 i2 -> do
    reset
    i1'   <- cse i1 
    reset
    i2'   <- cse i2
    reset
    next' <- cse (next i) 
    return i{ idesc = Icond tst i1' i2', next = next' }

cseExp i@Inst{ idesc = Iop op } = do
  r <- lookupInst op (args i)
  case r of
    Just idx  -> do
      table <- gets vntable
      case lookup idx (map (\(a,b) -> (b, a)) (M.toList table)) of
        Just r  -> do
          next' <- cse (next i)
          return i{
            idesc = Iop Imove,
            args  = [r],
            next = next'
            }

        Nothing -> do
          next' <- cse (next i)
          return i{ next = next' }

    Nothing -> do
      idx <- lift$newUniq
      valno <- mapM regNo (args i)
      modify $ \e -> e{
        adaq = M.insert Node{ operation = op, operands = valno } idx (adaq e)
        }
      setRegNo (result i!!0) idx
      next' <- cse(next i)
      return i{ next = next' }

lookupInst op args = do
  operands <- mapM regNo args
  table <- gets adaq
  return $ M.lookup Node{ operation = op, operands = operands } table 

regNo :: Reg -> P Index
regNo r = do 
  tab <- gets vntable
  case M.lookup r tab of
    Just n  -> return n
    Nothing -> do
      n <- lift$newUniq
      modify $ \e -> e{ vntable = M.insert r n tab }
      return n

setRegNo :: Reg -> Index -> P ()
setRegNo r i = modify $ \e -> e{ vntable = M.insert r i (vntable e) }


{- Peephole -}
peephole i@Inst{ idesc = Iend } = i
peephole i@Inst{
  idesc = Iop (Ihsr _),
  next = Inst{ idesc = Iop Imove }
  }
  | result i == args (next i)
  = i{ result = result (next i), next = peephole (next (next i)) }
peephole i@Inst{ idesc = Icond test ifso ifnot }
  = i{ idesc = Icond test (peephole ifso) (peephole ifnot),
       next  = peephole (next i) }
peephole i = i{ next = peephole (next i) }

fundecl f = do
  body' <- evalStateT (cse (fun_body f)) initEnv
  return f{ fun_body = {- peephole -} body' }
