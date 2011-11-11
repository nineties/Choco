------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Comballoc where

import Mach
import Reg

data AllocationState 
  = NoAlloc 
  | PendingAlloc Reg Int

allocatedSize NoAlloc = 0
allocatedSize (PendingAlloc _ n) = n

combine i allocstate =
  case idesc i of
    Iend -> (i, allocatedSize allocstate)
    Ireturn -> (i, allocatedSize allocstate)
    Iop (Ialloc size) ->
      case allocstate of
        NoAlloc ->
          let (newnext, newsize) = combine (next i) (PendingAlloc (result i !! 0) size)
          in (consInst (Iop (Ialloc newsize)) (result i) (args i) newnext, 0)
        PendingAlloc reg ofs ->
          let (newnext, newsize) = combine (next i) (PendingAlloc reg (ofs + size))
          in (consInst (Iop (Iintop_imm Iadd ofs))
                (result i) [reg] newnext,
              newsize)
    Iop Icall_ind ->
      let newnext = combine_restart (next i) in
      (consInst (idesc i) (result i) (args i) newnext,
       allocatedSize allocstate)

    Iop (Icall_imm _) ->
      let newnext = combine_restart (next i) in
      (consInst (idesc i) (result i) (args i) newnext,
       allocatedSize allocstate)

    Iop Itailcall_ind ->
      let newnext = combine_restart (next i) in
      (consInst (idesc i) (result i) (args i) newnext,
       allocatedSize allocstate)

    Iop (Itailcall_imm _) ->
      let newnext = combine_restart (next i) in
      (consInst (idesc i) (result i) (args i) newnext,
       allocatedSize allocstate)

    Iop op ->
      let (newnext, newsize) = combine (next i) allocstate
      in (consInst (idesc i) (result i) (args i) newnext, newsize)

    Icond tst ifso ifnot ->
      let newifso  = combine_restart ifso
          newifnot = combine_restart ifnot
          newnext  = combine_restart (next i)
      in (consInst (Icond tst newifso newifnot)
            (result i) (args i) newnext,
          allocatedSize allocstate)

combine_restart i = fst $ combine i NoAlloc


fundecl f = return f{ fun_body = combine_restart (fun_body f) }
