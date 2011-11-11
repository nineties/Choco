------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Linearize (
    Label, Inst(..), InstDesc(..),
    consInst, consNop, endInst,
    hasFallthrough, 
    FunDec(..),
    fundecl
    ) where

import Choco
import Id
import Reg
import qualified Mach as M
import qualified CmmSyn as Cmm
import qualified Data.Set as S
import Outputable
import Panic

import Control.Monad.State

type Label = Int

data Inst = Inst {
    idesc   :: InstDesc,
    result  :: [Reg],
    args    :: [Reg],
    live    :: S.Set Reg,
    next    :: Inst
    }
    deriving (Eq, Show)

{- for test -}

data InstDesc
    = Lend
    | Lop M.Operation
    | Lnop
    | Lreturn
    | Llabel Label
    | Lbranch Label
    | Lcondbranch M.Test Label
    deriving (Eq, Show)

instance Outputable Inst where
  ppr i = case idesc i of
    Lend  -> empty
    Lop op -> M.pprOp op (result i) (args i) $$ ppr (next i)
    Lnop   -> text "nop" $$ ppr (next i)
    Lreturn -> text "return" $$ ppr (next i)
    Llabel lbl  ->
      text "===" <+> text "label-" <> int lbl <+> text "=="
       $$ ppr (next i)
    Lbranch lbl -> text "branch" <+> text "lablel-" <> int lbl $$ ppr (next i)
    Lcondbranch t lbl -> 
      hang (text "if" <+> M.pprTest t (args i)) 2 
        (text "jump" <+> text "label-" <> int lbl) 
        $$ ppr (next i)

newLabel = lift $ newUniq

getLabel inst = case idesc inst of
    Lbranch lbl -> return (lbl, inst)
    Llabel lbl  -> return (lbl, inst)
    Lend        -> return (-1, inst)
    _ -> do
      lbl <- newLabel
      return (lbl, consSimpleInst (Llabel lbl) inst)

hasFallthrough inst 
    = case inst of
           Lreturn               -> False
           Lbranch _             -> False
           Lop M.Itailcall_ind     -> False
           Lop (M.Itailcall_imm _) -> False
           _                     -> True

endInst = Inst {
    idesc   = Lend,
    result  = [],
    args    = [],
    live    = S.empty,
    next    = endInst
    }

consNop n = Inst {
    idesc  = Lnop,
    result = [],
    args   = [],
    live   = S.empty,
    next   = n
    }

consInst d r a n = Inst {
    idesc  = d,
    result = r,
    args   = a,
    live   = S.empty,
    next   = n
    }
    
consSimpleInst d n = Inst {
    idesc  = d,
    result = [],
    args   = [],
    live   = S.empty,
    next   = n
    }

data FunDec = FunDec {
    fun_name :: String,
    fun_body :: Inst
    }
    deriving (Eq)

instance Outputable FunDec where
  ppr f = hang (text "function" <+> text (fun_name f)) 2 (ppr $ fun_body f) 

data Env = Env 
defaultEnv = Env

type P a = StateT Env ChocoM a

copyInst :: InstDesc -> M.Inst -> Inst -> Inst
copyInst d i n = Inst {
    idesc  = d,
    next   = n,
    args   = M.args i,
    result = M.result i,
    live   = M.live i
    }

addBranch :: Label -> Inst -> P Inst
addBranch lbl n =
    if lbl >= 0 
       then do
        n1 <- discardDeadCode n
        case idesc n1 of
             Llabel lbl1 | lbl1 == lbl  -> 
                return n1

             _  -> 
                return $ consSimpleInst (Lbranch lbl) n1
       else
        discardDeadCode n

discardDeadCode :: Inst -> P Inst
discardDeadCode n = case idesc n of
    Lend        -> return n
    Llabel _    -> return n
    _           -> discardDeadCode (next n)

linear :: M.Inst -> Inst -> P Inst
linear inst last = case M.idesc inst of
    M.Iend -> return last

    M.Iop M.Itailcall_ind -> 
        discardDeadCode last >>=
        return . copyInst (Lop M.Itailcall_ind) inst 

    M.Iop (M.Itailcall_imm s) -> 
        discardDeadCode last >>=
        return . copyInst (Lop (M.Itailcall_imm s)) inst

    M.Iop op 
      | op `elem` [M.Imove, M.Ireload, M.Ispill] && 
        loc (M.args inst!!0) == loc (M.result inst!!0) -> 
        linear (M.next inst) last

    M.Iop op ->
        linear (M.next inst) last >>=
        return . copyInst (Lop op) inst

    M.Ireturn ->
        discardDeadCode last >>=
        return . copyInst Lreturn inst

    M.Icond test ifso ifnot -> do
        n1 <- linear (M.next inst) last
        case (M.idesc ifso, M.idesc ifnot, idesc n1)  of
             (M.Iend, _, Lbranch lbl) -> 
                return . copyInst (Lcondbranch test lbl) inst 
                          =<< linear ifnot n1
             
             (_, M.Iend, Lbranch lbl) ->
                return . copyInst (Lcondbranch (M.invertTest test) lbl) inst
                          =<< linear ifso n1

             (M.Iend, _, _) -> do
                 (lbl_end, n2) <- getLabel n1
                 return . copyInst (Lcondbranch test lbl_end) inst
                          =<< linear ifnot n2

             (_, M.Iend, _) -> do
                 (lbl_end, n2) <- getLabel n1
                 return . copyInst (Lcondbranch (M.invertTest test) lbl_end) inst
                          =<< linear ifso n2

             {- Should attempt branch prediction here -}
             (_, _, _) -> do
                 (lbl_end, n2)     <- getLabel n1
                 (lbl_else, nelse) <- getLabel 
                          =<< linear ifnot n2
                 
                 return . copyInst (Lcondbranch (M.invertTest test) lbl_else) inst
                          =<< linear ifso =<< addBranch lbl_end nelse


    M.Iloop body -> do
        lbl_head <- newLabel
        n1 <- linear (M.next inst) last
        n2 <- linear body (consSimpleInst (Lbranch lbl_head) n1)
        return $ consSimpleInst (Llabel lbl_head) n2

fundecl fun = do
  fun_body <- evalStateT (linear (M.fun_body fun) endInst) 
                    defaultEnv
  return FunDec {
    fun_name = M.fun_name fun,
    fun_body = fun_body
    }
