------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Coloring where


import Choco
import Mach
import Outputable
import Proc
import Reg
import RegM

import Control.Monad.State
import Data.Array.IO
import Data.Function
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

{- Register allocation by coloring of the interference graph -}
type P a = StateT Env ChocoM a
data Env = Env {
  constrained    :: S.Set Reg,
  unconstrained  :: S.Set Reg,
  best_score     :: Int,
  best_reg       :: Int,
  start_register :: Int
  }

initEnv = Env {
  constrained   = S.empty,
  unconstrained = S.empty,
  best_score    = -1000000,
  best_reg      = -1,
  start_register = normalRegFirst
  }
-- Preallocation of spilled registers in the stack
allocateSpilled :: Reg -> P ()
allocateSpilled reg = do
  r   <- lift $ getsRegInfo reg spill
  int <- lift $ getsRegInfo reg interf
  when r $ do
    nslots <- lift $ gets num_stack_slots
    conflict <- foldM 
      (\c r -> do
        r_loc <- lift $ getsRegInfo r location
        case r_loc of
          Stack (Local n) -> return (M.insert n True c)
          _               -> return c)
      (foldr (\n -> M.insert n False) M.empty [0..nslots])
      int
    let slot =  length . fst . span (== True) .  map snd . sortBy (compare `on` fst) $ M.toList conflict
    -- let slot = length . filter (== True) . map snd $ M.toList conflict
    lift $ modifyRegInfo reg $ \i -> i{ location = Stack (Local slot) }
    when (slot >= nslots) $
      lift $ modify $ \e -> e{ num_stack_slots = slot + 1 }

-- Compute the degree (= number of neighbours of the same type)
-- of each register, and split them in two sets:
-- unconstrained (degree < number of available registers)
-- and constrained (degree >= number of available registers).
-- Spilled registers are ignored in the process.
findDegree reg = do
  RegInfo{ spill = s, interf = int } <- lift $ getRegInfo reg
  when (not s) $ do
    let avail_regs = numNormalReg
    deg <- foldM (\d r -> do
      s' <- lift $ getsRegInfo reg spill
      if not s' then return (d + 1) else return d)
      0 int
    lift $ modifyRegInfo reg $ \i -> i{ degree = deg }
    if deg >= avail_regs
      then modify $ 
          \e -> e{ constrained = S.insert reg (constrained e) } 
      else modify $ 
          \e -> e{ unconstrained = S.insert reg (unconstrained e) } 

-- Remove a register from the interference graph
removeReg reg = do
  -- 0 means reg is no longer part of the graph
  lift $ modifyRegInfo reg $ \i -> i{ degree = 0 }
  int <- lift $ getsRegInfo reg interf
  mapM_ (\r -> do
    d <- lift $ getsRegInfo r degree
    when (d > 0) $ do
      let olddeg = d
      lift $ modifyRegInfo reg $ \i -> i{ degree = olddeg - 1 }
      when (olddeg == numNormalReg) $ do
        modify $ \e -> e{
          constrained   = S.delete r (constrained e),
          unconstrained = S.insert r (unconstrained e)
          }
    ) int

-- Remove all registers one by one, unconstrained if possible, otherwise
-- constrained with lowest spill cost. Return the list of registers removed
-- in reverse order.
-- The spill cost measure is [spillCost / degree].
-- spillCost estimates the number of accesses to thie registers
removeAllRegs stack = do
  uc <- gets unconstrained
  c  <- gets constrained
  if (not $ S.null uc) 
    then do
      -- pick up any unconstrained register
      let r = head (S.toList uc)
      modify $ \e -> e{ unconstrained = S.delete r uc }
      removeAllRegs (r:stack)
    else if (not $ S.null c)
      then do
        (r, _, _) <- foldM 
          (\(r, min_degree, min_spill_cost) r2 -> do
            r2_sc <- lift $ getsRegInfo r2 spillCost
            r2_d  <- lift $ getsRegInfo r2 degree
            if r2_sc * min_degree < min_spill_cost * r2_d
              then return (r2, r2_d, r2_sc)
              else return (r, min_degree, min_spill_cost)
            ) (emptyReg, 0, 1) (S.toList c)
        modify $ \e -> e{ constrained = S.delete r c }
        removeAllRegs (r:stack)
      else return stack -- all registers have been removed

-- Iterate over all registers preferred by the given register
iterPreferred f reg = do
  lift $ modifyRegInfo reg $ \i -> i{ visited = True }

  p <- lift $ getsRegInfo reg prefer
  mapM (\(r, w) -> walk r w) p

  lift $ modifyRegInfo reg $ \i -> i{ visited = False }
  where
  walk r w = do
    v <- lift $ getsRegInfo r visited
    when (not v) $ do
      f r w
      p <- lift $ getsRegInfo r prefer
      case p of
       [] -> return ()
       p' -> do lift $ modifyRegInfo r $ \i -> i{ visited = True }
                mapM (\(r1, w1) -> walk r1 (min w w1)) p'
                lift $ modifyRegInfo r $ \i -> i{ visited = False }

-- Assign a location to a register, the best we can
assignLocation reg = do
  let first_reg = normalRegFirst
  let num_regs  = numNormalReg
  let last_reg  = first_reg + num_regs
  score <- lift.runIO $ 
    newArray (0, num_regs-1) 0 :: P (IOArray Int Int)
  modify $ \e -> e{
    best_score = -1000000,
    best_reg   = -1
    }
  start <- gets start_register
  when (num_regs > 0) $ do
    iterPreferred (\r w -> do
      r_loc <- lift $ getsRegInfo r location
      case r_loc of
        Register n -> when (n >= first_reg && n < last_reg) $ do
                        lift $ runIO $ readArray score (n - first_reg)
                          >>= writeArray score (n - first_reg) . (+ w)
        Unknown -> do
          int <- lift $ getsRegInfo r interf
          mapM_ (\neighbour -> do
            n_loc <- lift $ getsRegInfo neighbour location
            case n_loc of
              Register n -> when (n >= first_reg && n < last_reg) $ do
                            lift $ runIO $ readArray score (n - first_reg)
                              >>= writeArray score (n - first_reg) . (+ w)
              _ -> return ()) int
        _ -> return ()) reg

    int <- lift $ getsRegInfo reg interf
    mapM_ (\neighbour -> do
      n_loc <- lift $ getsRegInfo neighbour location
      case n_loc of
        Register n -> when (n >= first_reg && n < last_reg) $ do
                    lift $ runIO $ writeArray score (n - first_reg) (-1000000)
        _ -> return ()

      iterPreferred (\r w -> do
        r_loc <- lift $ getsRegInfo r location
        case r_loc of
          Register n -> when (n >= first_reg && n < last_reg) $ do
                          lift $ runIO $ readArray score (n - first_reg)
                            >>= writeArray score (n - first_reg) . (subtract (w-1))
          _ -> return ()) neighbour) int
    score_list <- lift.runIO $ getAssocs score
    let (low, high) = splitAt start score_list
    let (best_reg1, best_score1) = foldl (\(n1, s1) (n2, s2) -> if s1 > s2 then (n1, s1) else (n2, s2)) (-1, -1000000) high
    let (best_reg2, best_score2) = foldl (\(n1, s1) (n2, s2) -> if s1 > s2 then (n1, s1) else (n2, s2)) (best_reg1, best_score1) low

    modify $ \e -> e{ best_score = best_score2, best_reg = best_reg2 }

  -- Found a register?
  bestr <- gets best_reg
  if bestr >= 0 
    then do
      lift $ modifyRegInfo reg $ \i -> i{ location = Register (first_reg + bestr) }
      modify$ \e -> e{ start_register 
        = ((start + 1) `mod` num_regs) + normalRegFirst
        }
    else do
      nslots <- lift $ gets num_stack_slots
      score2 <- lift $ runIO $ 
                  newArray (0, nslots-1) 0 :: P (IOArray Int Int)
      p <- lift $ getsRegInfo reg prefer
      mapM_ (\(r, w) -> do
        r_loc <- lift $ getsRegInfo r location
        r_int <- lift $ getsRegInfo r interf
        case r_loc of
          Stack (Local n) 
            -> lift $ runIO $ readArray score2 n 
                  >>= writeArray score2 n . (+ w)
          Unknown
            -> mapM_ (\neighbour -> do
                n_loc <- lift $ getsRegInfo neighbour location
                case n_loc of
                  Stack (Local n) 
                    -> lift $ runIO $ readArray score2 n 
                          >>= writeArray score2 n . (+ w)
                  _ -> return ()) r_int
          _ -> return ()) p

      int <- lift $ getsRegInfo reg interf
      mapM_ (\neighbour -> do
        n_loc <- lift $ getsRegInfo neighbour location
        case n_loc of
          Stack (Local n) -> lift $ runIO $ writeArray score2 n (-1000000)
          _ -> return ()

        mapM_ (\(r, w) -> do
          r_loc <- lift $ getsRegInfo r location
          p     <- lift $ getsRegInfo neighbour prefer
          case r_loc of
            Stack (Local n) 
                  -> lift $ runIO $ readArray score2 n 
                        >>= writeArray score2 n . (+ w)
            _ -> return ()) p
            ) int

      modify $ \e -> e{ best_score = -1000000, best_reg = -1 }
      score_list <- lift $ runIO $ getAssocs score2
      let (best_slot, best_score) = foldl1 (\(n1, s1) (n2, s2) -> if s1 > s2 then (n1, s1) else (n2, s2)) score_list

      
      if best_score >= 0
        then lift $ modifyRegInfo reg $ 
                \i -> i{ location = Stack (Local best_slot) }
        else do -- Allocate a new stack slot
             nslots <- lift $ gets num_stack_slots
             lift $ modifyRegInfo reg $
                \i -> i{ location = Stack (Local nslots) }
             lift $ modify $ \e -> e{ num_stack_slots = nslots + 1 }
  lift $ modifyRegInfo reg $ \i -> i{ prefer = [] }


allocateRegisters fd = do
  -- First pass  : preallocate spill registers
  -- Second pass : compute the degrees
  -- Third pass  : determine coloring order by successive removals of regs
  -- Fourth pass : assign registers in that order
  modify (\e -> e{ num_stack_slots = 0 })

  reg_list <- gets reg_list
  execStateT (do 
    mapM_ allocateSpilled reg_list
    mapM_ findDegree reg_list
    removeAllRegs [] >>= mapM_ assignLocation 
    ) initEnv
  fd' <- updateRegisterLocation fd
  nslots <- gets num_stack_slots

  regs <- destroyedRegisters fd'
  modify $ \e -> e{
    fun_reg_info = M.insert (fun_name fd') regs (fun_reg_info e)
    }

  return (fd', nslots)
