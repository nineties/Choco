------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Reload where

import Choco
import Mach
import Outputable
import Panic
import Reg
import RegM

import Control.Monad.State

type P a = StateT Bool ChocoM a

accessStack :: [Reg] -> Bool
accessStack (Reg{ loc = Stack _ }:rs)  = True
accessStack (_:rs) = accessStack rs
accessStack []     = False

insertMove dst src next
  | loc src == loc dst  = next
  | otherwise = consInst (Iop Imove) [dst] [src] next

insertMoves dst src next
  = foldr (\(d, s) n -> insertMove d s n) next (zip dst src)

makeReg r = case loc r of
  Unknown -> lift$simpleError (text "Reload:makeReg")
  Register _   -> return r
  Stack _ -> do
    put True
    newr <- lift$copyReg r
    lift$modifyRegInfo newr $ \i -> i{ spillCost = 100000 }
    return newr

makeRegs rv = mapM makeReg rv

makeReg1 (r:rv) = makeReg r >>= return . (: rv)

reloadOperation op res arg = case op of
  _ | op `elem` [Imove, Ireload, Ispill]
    -> case (res !! 0, arg !! 0) of
      (Reg{ loc = Stack s1 }, Reg{ loc = Stack s2 }) | s1 /= s2
        -> do r <- makeReg (arg!!0)
              return (res, [r])
      _ -> return (res, arg)
  _ -> do arg' <- makeRegs arg
          res' <- makeRegs res
          return (res', arg')

reload i = case idesc i of
  Iend  -> return i
  Ireturn -> return i
  Iop (Itailcall_imm _) -> return i
  Iop Itailcall_ind -> do
    newarg <- makeReg1 (args i)
    return $ insertMoves newarg (args i) $
       consInstLive (idesc i) (result i) newarg (live i) (next i)
  Iop (Icall_imm _) -> do
    next' <- reload (next i)
    return $ consInstLive (idesc i) (result i) (args i) (live i) next'
  Iop Icall_ind -> do
    newarg <- makeReg1 (args i)
    next' <- reload (next i)
    return $ insertMoves newarg (args i) $
       consInstLive (idesc i) (result i) newarg (live i) next'

  Iop op -> do
    next' <- reload (next i)
    (newres, newarg) <- reloadOperation op (result i) (args i)
    return $ insertMoves newarg (args i) $
      consInstLive (idesc i) newres newarg (live i) $
        insertMoves (result i) newres next'

  Icond test ifso ifnot -> do
    newarg <- makeRegs (args i)
    ifso' <- reload ifso
    ifnot' <- reload ifnot
    next' <- reload (next i)
    return $ insertMoves newarg (args i) $
      consInst (Icond test ifso' ifnot') [] newarg (next') 

fundecl f = evalStateT (do
  new_body <- reload (fun_body f)
  redo_regalloc <- get
  return (f{ fun_body = new_body }, redo_regalloc)
  ) False
