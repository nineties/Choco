------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Mach (
    IntOperation(..), Test(..),
    Operation(..),
    Inst(..), InstDesc(..),
    instLength,
    invertTest,
    emptyInst, endInst,
    FunDec(..),
    consInst, consInstLive,
    instIter,
    pprOp, pprTest,
    destroyedRegisters
    ) where

import Arch
import Choco
import Reg
import Outputable
import qualified CmmSyn as Cmm
import qualified Data.Set as S

import Control.Monad.State
import qualified Data.Map as M

{- representation of machine code -}

data IntOperation
    = Iadd | Isub | Imul 
    | Isra | Isll
    deriving (Eq, Ord, Show)

instance Outputable IntOperation where
  ppr Iadd  = char '+'
  ppr Isub  = char '-'
  ppr Imul  = char '*'
  ppr Isra  = char '>'
  ppr Isll  = char '<'

data Test
    = Itruetest
    | Ifalsetest
    | Iinttest Cmm.Comp
    | Iinttest_imm Cmm.Comp Int
    | Ifloattest Cmm.Comp Bool
    deriving (Eq, Show)

invertTest Itruetest = Ifalsetest
invertTest Ifalsetest = Itruetest
invertTest (Iinttest cmp) = Iinttest (Cmm.negateComp cmp)
invertTest (Iinttest_imm cmp i) = Iinttest_imm (Cmm.negateComp cmp) i
invertTest (Ifloattest cmp neg) = Ifloattest cmp (not neg)

data Operation
    = Imove
    | Ispill
    | Ireload
    | Iconst_int Int
    | Iconst_float Float
    | Iconst_symbol String
    | Icall_ind
    | Icall_imm String
    | Itailcall_ind
    | Itailcall_imm String
    | Istackoffset Int
    | Iload Arch.AddressingMode 
    | Istore Arch.AddressingMode
    | Ialloc Int
    | Iintop IntOperation
    | Iintop_imm IntOperation Int
    | Ifabs | Ifneg | Isqrt | Ifinv
    | Ifadd | Ifsub | Ifmul | Iftoi | Iitof
    | Iput | Iget
    | Ihsw Int | Ihsr Int
    deriving (Eq, Ord, Show)

pprOp op res args = case op of
  Imove -> move (ppr (res!!0)) (ppr (args!!0))
  Ispill -> move (ppr (res!!0)) (ppr (args!!0)) <+> text "(spill)"
  Ireload -> move (ppr (res!!0)) (ppr (args!!0)) <+> text "(reload)"
  Iconst_int n -> move (ppr (res!!0)) (int n)
  Iconst_float f -> move (ppr (res!!0)) (float f)
  Iconst_symbol s -> move (ppr (res!!0)) (text s)
  Icall_ind -> text "call" <+> hsep (map ppr args)
  Icall_imm s -> text "call" <+> text s <+> hsep (map ppr args)
  Itailcall_ind -> text "tailcall" <+> hsep (map ppr args)
  Itailcall_imm s -> text "tailcall" <+> text s <+> hsep (map ppr args)
  Istackoffset s  
    | s > 0 -> text "%sp +=" <+> int s
    | s < 0 -> text "%sp -=" <+> int (-s)
  Iload addr -> ppr (res!!0) <+> text ":=" <+> memloc2 addr args
  Istore addr -> memloc addr args <+> text ":=" <+> ppr (args!!0)
  Ialloc n -> ppr (res!!0) <+> text ":= alloc" <+> int n
  Iintop op -> ppr (res!!0) <+> text ":=" <+> ppr (args!!0) <+> ppr op <+> ppr (args!!1)
  Iintop_imm op n -> ppr (res!!0) <+> text ":=" <+> ppr (args!!0) <+> ppr op <+> int n
  Ifabs -> ppr1 "fabs"
  Ifneg -> ppr1 "fneg"
  Isqrt -> ppr1 "fsqrt"
  Ifinv -> ppr1 "finv"
  Ifadd -> ppr2 "+."
  Ifsub -> ppr2 "-."
  Ifmul -> ppr2 "*."
  Iftoi -> ppr1 "ftoi"
  Iitof -> ppr1 "itof"
  Iput  -> text "put" <+> ppr (args!!0)
  Iget  -> ppr (res!!0) <+> text "<- get"
  Ihsw n -> text "hsw" <> int n <+> ppr (args!!0) 
  Ihsr n -> ppr (res!!0) <+> text "hsr" <> int n
  where
  memloc (Ibased lbl offs) _
    | offs >= 0  = brackets(text lbl <+> char '+' <+> int offs)
    | offs < 0   = brackets(text lbl <+> char '-' <+> int (-offs))
  memloc (Iindexed idx) r
    | idx >= 0  = brackets(ppr (r!!1) <+> char '+' <+> int idx)
    | idx < 0   = brackets(ppr (r!!1) <+> char '-' <+> int (-idx)) 

  memloc2 (Ibased lbl offs) _
    | offs >= 0  = brackets(text lbl <+> char '+' <+> int offs)
    | offs < 0   = brackets(text lbl <+> char '-' <+> int (-offs))
  memloc2 (Iindexed idx) r
    | idx >= 0  = brackets(ppr (r!!0) <+> char '+' <+> int idx)
    | idx < 0   = brackets(ppr (r!!0) <+> char '-' <+> int (-idx)) 

  move a b = a <+> text ":=" <+> b
  ppr1 op = move (ppr (res!!0)) (text op <+> ppr (args!!0))
  ppr2 op = move (ppr (res!!0)) (ppr (args!!0) <+>
    text op <+> ppr (args!!1))

pprTest test args = case test of
  Itruetest -> ppr (args!!0) <+> text "== true"
  Ifalsetest -> ppr (args!!0) <+> text "== false"
  Iinttest cmp -> ppr (args!!0) <+> ppr cmp <+> ppr (args!!1)
  Iinttest_imm cmp n -> ppr (args!!0) <+> ppr cmp <+> int n
  Ifloattest c True -> ppr (args!!1) <+> ppr c <+> ppr (args!!0)
  Ifloattest c False -> ppr (args!!0) <+> ppr c <+> ppr (args!!1)

data Inst = Inst {
    idesc   :: InstDesc,
    result  :: [Reg],
    args    :: [Reg],
    live    :: S.Set Reg,

    next    :: Inst
    }

instLength :: Inst -> Int
instLength i = case idesc i of
  Iend          -> 0
  Ireturn       -> 1
  Iop Itailcall_ind -> 1
  Iop (Itailcall_imm _) -> 1
  Icond _ i1 i2 -> instLength i1 + instLength i2 + 1
  _ -> 1 + instLength (next i)

instance Eq Inst where
  Inst{ idesc = Iend } == Inst{ idesc = Iend } = True
  Inst{ idesc = Iend } == _ = False
  _ == Inst{ idesc = Iend } = False
  i1 == i2 = idesc i1 == idesc i2 && result i1 == result i2 &&
             args i1 == args i2 && live i1 == live i2 &&
             next i1 == next i2

instance Outputable Inst where
  ppr Inst{ idesc = Iend }  = empty
  ppr i@Inst{ idesc = Icond test ifso ifnot }
    = (hang (text "if" <+> pprTest test (args i)) 2 (vcat 
          [hang (text "then") 2 (ppr ifso),
           hang (text "else") 2 (ppr ifnot)]
        )) $$ ppr (next i)
  ppr i@Inst{ idesc = Iop op }
    = pprOp op (result i) (args i) $$ ppr (next i) 
  ppr i@Inst{ idesc = Ireturn }
    = text "return" $$ ppr (next i)

instance Show Inst where
  show = show.ppr
data InstDesc
    = Iend
    | Iop Operation
    | Ireturn
    | Icond Test Inst Inst
    | Iloop Inst
    deriving (Eq)

data FunDec = FunDec {
    fun_name :: String,
    fun_args :: [Reg],
    fun_body :: Inst
    }
    deriving (Eq)

instance Outputable FunDec where
  ppr f = hang (text "function" <+> text (fun_name f) <+> parens (hsep $ map ppr (fun_args f))) 2 (ppr (fun_body f))

emptyInst = Inst {
    idesc   = Iend,
    args    = [],
    result  = [],
    live    = S.empty,

    next    = emptyInst
    }

endInst = emptyInst

consInst d r a n = Inst {
  idesc  = d,
  result = r,
  args   = a,
  live   = S.empty,
  next   = n
  }

consInstLive d r a l n = Inst {
  idesc  = d,
  result = r,
  args   = a,
  live   = l,
  next   = n
  } 
instIter m i =
  case (idesc i) of
    Iend  -> return i
    _ -> do
      i' <- m i
      case (idesc i') of
        Iend  -> return i'
        Ireturn -> return i'
        Iop Itailcall_ind -> return i'
        Iop (Itailcall_imm _) -> return i'
        Icond tst ifso ifnot -> do
          ifso'  <- instIter m ifso
          ifnot' <- instIter m ifnot
          next'  <- instIter m (next i')
          return i'{
            idesc = Icond tst ifso' ifnot',
            next  = next'
            }
        _ -> do
          next' <- instIter m (next i')
          return i'{
            next = next'
            }


destroyedRegisters :: FunDec -> ChocoM [Reg]
destroyedRegisters f = do
  rs <- walk S.empty (fun_body f) >>= return . S.toList
  return rs
  where
  walk rs i = case idesc i of
    Iend  -> return rs
    Ireturn -> return rs
    Icond _ i1 i2 
      -> do
      rs1 <- walk rs i1
      rs2 <- walk rs i2
      return (rs1 `S.union` rs2)

    Iop Icall_ind -> return . S.fromList =<< gets phys_regs
    Iop (Icall_imm s) -> do
      table <- gets fun_reg_info
      case M.lookup s table of
        Just rs2 -> walk (rs `S.union` (S.fromList rs2)) (next i)
        Nothing  -> do
          return . S.fromList =<< gets phys_regs

    Iop Itailcall_ind -> return rs
    Iop (Itailcall_imm _) -> return rs
    Iop _ -> walk (rs `S.union` (S.fromList (result i))) (next i)


