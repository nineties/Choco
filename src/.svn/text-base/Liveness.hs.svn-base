------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Liveness (fundecl) where

{- liveness analysis -}

import Choco
import Mach
import Outputable
import Panic
import Proc
import Reg

import Control.Monad.State
import qualified Data.Set as S

isNormal r = case loc r of
  Register r  -> normalRegFirst <= r && r < normalRegEnd
  _ -> True

normalArgs i = filter isNormal (args i)

liveness i@Inst{ idesc = Iend } finally
    = (i{ live = finally }, finally)

liveness i@Inst{ idesc = Ireturn } finally
    = (i, S.fromList (normalArgs i))

liveness i@Inst{ idesc = Iop Itailcall_ind } finally
    = (i, S.fromList (normalArgs i))

liveness i@Inst{ idesc = Iop (Itailcall_imm _) } finally
    = (i, S.fromList (normalArgs i))

liveness i@Inst{ idesc = Icond test ifso ifnot } finally
    = let
      (next', at_join)   = liveness (next i) finally
      (ifso', at_fork1)  = liveness ifso at_join
      (ifnot', at_fork2) = liveness ifnot at_join
      at_fork = S.union at_fork1 at_fork2
      in (
        i{ idesc = Icond test ifso' ifnot', live  = at_fork, next = next' },
        S.union at_fork (S.fromList (normalArgs i))
        )

liveness i@Inst{ idesc = Iloop body } finally
    = let
      (body', at_top) = walk body S.empty
      in (
        i{ idesc = Iloop body', live  = at_top },
        at_top
        )
    where
    walk body set = 
        let (body', set') = liveness body set
            newset = S.union set set'
        in if newset == set
                then (body', newset)
                else walk body' newset

liveness i finally
  = let (next', set) = liveness (next i) finally
        across = set S.\\ (S.fromList $ result i)
    in (i{ live = across, next = next' }, 
          S.union across (S.fromList $ normalArgs i))
      

fundecl fun = do
    let (body', initially_live) = liveness (fun_body fun) S.empty
    {- Sanity check: only function parameters can be live at entrypoint -}
        wrong_live = initially_live S.\\ (S.fromList (fun_args fun))
    if not (S.null wrong_live)
           then do
           simpleError $ text "wrong live variables:" <+> hsep (map ppr (S.toList wrong_live))
           else return fun{ fun_body = body' }
