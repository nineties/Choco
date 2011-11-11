------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Interf where

import Choco
import Mach ( Inst(..), InstDesc(..), Operation(..), FunDec(..) )
import Outputable
import Proc
import Reg
import RegM

import Control.Monad.State
import qualified Data.Set as S

type P a = StateT (S.Set (Int, Int)) ChocoM a



-- Record an interference between two registers
addInterf :: Reg -> Reg -> P ()
addInterf ri rj = do
  let i = stamp ri;  j = stamp rj
  when (i /= j) $ do
    let pair = if i < j then (i, j) else (j, i)
    mat <- get
    when (S.notMember pair mat) $ do
      modify $ S.insert pair
      lift $ modifyRegInfo ri $ \i -> i{ interf = rj : (interf i) }
      lift $ modifyRegInfo rj $ \i -> i{ interf = ri : (interf i) }

-- Record interferences between register lists
addInterfSet :: [Reg] -> S.Set Reg -> P ()
addInterfSet v s = mapM_ (uncurry addInterf) 
                      [(x, y)| x <- v, y <- S.toList s]

-- Record interferences between elements of a list
addInterfSelf :: [Reg] -> P ()
addInterfSelf (x:xs) = do mapM (addInterf x) xs 
                          addInterfSelf xs
addInterfSelf [] = return ()

-- Record interferences between the destination of a move and a set
-- of live registers. Since the destination is equal to the source,
-- do not add an interference between them if the source is still live
addInterfMove :: Reg -> Reg -> S.Set Reg -> P ()
addInterfMove src dst live 
  = mapM_ (\r -> when (stamp r /= stamp src) (addInterf dst r)) 
        (S.toList live)


-- Compute interferences
computeInterf :: Inst -> P ()
computeInterf i = do
  destroyed <- lift$destroyedAtOper (idesc i)
  when (not (null destroyed)) $
    addInterfSet destroyed (live i)
  case (idesc i) of
    Iend    -> return ()
    Ireturn -> return ()
    Iop op | op `elem` [Imove, Ispill, Ireload]
      -> do addInterfMove (args i !! 0) (result i !! 0) (live i)
            computeInterf (next i)
    Iop Itailcall_ind   -> return ()
    Iop (Itailcall_imm _) -> return ()
    Iop op 
      -> do addInterfSet (result i) (live i)
            addInterfSelf (result i)
            computeInterf (next i)

    Icond tst ifso ifnot
      -> do computeInterf ifso
            computeInterf ifnot
            computeInterf (next i)

-- Add a preference from one reg to another.
-- Do not add anything if the two registers conflict,
-- or if the source register already has a location.
addPref weight ri rj =
  when (weight > 0) $ do
    let i = stamp ri; j = stamp rj
    mat <- get
    when (i /= j && loc ri == Unknown &&
          (let p = if i < j then (i, j) else (j, i) in S.notMember p mat)) $ do
          lift $ modifyRegInfo ri $ \i -> i{ prefer = (rj, weight) : prefer i }


-- Add a mutual preference between two regs
addMutualPref weight ri rj = addPref weight ri rj >> addPref weight rj ri


-- Update the spill cost of the registers involved in an operation
addSpillCost cost args =
  mapM_ (\r -> 
    lift $ modifyRegInfo r $ \i -> i{ spillCost = (spillCost i) + cost })
    args

-- Compute preference and spill costs
computePrefer w i = do
  addSpillCost w (args i)
  addSpillCost w (result i)
  case idesc i of
    Iend  -> return ()
    Ireturn -> return ()
    Iop Imove -> do
      addMutualPref w (args i !! 0) (result i !! 0)
      computePrefer w (next i)
    Iop Ispill -> do
      addPref (w `div` 4) (args i !! 0) (result i !! 0)
      computePrefer w (next i)
    Iop Ireload -> do
      addPref (w `div` 4) (result i !! 0) (args i !! 0)
      computePrefer w (next i)
    Iop Itailcall_ind -> return ()
    Iop (Itailcall_imm _) -> return ()
    Iop op -> computePrefer w (next i)
    Icond tst ifso ifnot -> do
      computePrefer (w `div` 2) ifso
      computePrefer (w `div` 2) ifnot
      computePrefer w (next i)

buildGraph fun = 
  evalStateT (computeInterf (fun_body fun) >> computePrefer 8 (fun_body fun)) S.empty
