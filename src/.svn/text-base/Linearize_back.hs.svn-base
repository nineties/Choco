------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Linearize (
    Label, Inst(..), InstDesc(..),
    consInst, consNop, endInst,
    newLabel,
    hasFallthrough, 
    FunDec(..),
    linearizeFunDec
    ) where

import Id
import Reg
import qualified Mach as M
import qualified CmmSyn as Cmm
import qualified Data.Set as S
import Panic

import Control.Applicative
import Control.Monad.State

type Label = Int

data Inst = Inst {
    idesc   :: InstDesc,
    result  :: [Reg],
    args    :: [Reg],
    live    :: S.Set Reg,
    next    :: Inst
    }
    deriving (Eq)

{- for test -}
instance Show Inst where
    show Inst{idesc = Lend} = ""
    show (Inst i r a _ n) =
        show i ++ " "++ show r ++ " "++ show a ++ "\n" ++ show n


data InstDesc
    = Lend
    | Lop M.Operation
    | Lnop
    | Lreturn
    | Llabel Label
    | Lbranch Label
    | Lcondbranch M.Test Label
    deriving (Eq, Show)

labelCounter = genIdSource 99
newLabel _ = newId labelCounter

getLabel inst = case idesc inst of
    Lbranch lbl -> (lbl, inst)
    Llabel lbl  -> (lbl, inst)
    Lend        -> (-1, inst)
    _ -> let lbl = newLabel() in (lbl, consSimpleInst (Llabel lbl) inst)

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
    fun_body :: Inst,
    fun_fast :: Bool
    }
    deriving (Eq, Show)

data LinearEnv = LinearEnv {
    }

defaultLinearEnv = LinearEnv {}

type Linearizer a = State LinearEnv a

copyInst :: InstDesc -> M.Inst -> Inst -> Inst
copyInst d i n = Inst {
    idesc  = d,
    next   = n,
    args   = M.args i,
    result = M.result i,
    live   = M.live i
    }

addBranch :: Label -> Inst -> Linearizer Inst
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

discardDeadCode :: Inst -> Linearizer Inst
discardDeadCode n = case idesc n of
    Lend        -> return n
    Llabel _    -> return n
    _           -> discardDeadCode (next n)

linear :: M.Inst -> Inst -> Linearizer Inst
linear inst last = case M.idesc inst of
    M.Iend -> return last

    M.Iop M.Itailcall_ind ->
        copyInst (Lop M.Itailcall_ind) inst <$> discardDeadCode last

    M.Iop (M.Itailcall_imm s) ->
        copyInst (Lop (M.Itailcall_imm s)) inst <$> discardDeadCode last

    M.Iop op ->
        copyInst (Lop op) inst <$> linear (M.next inst) last

    M.Ireturn ->
        copyInst Lreturn inst <$> discardDeadCode last

    M.Icond test ifso ifnot -> do
        n1 <- linear (M.next inst) last
        case (M.idesc ifso, M.idesc ifnot, idesc n1)  of
             {- Should attempt branch prediction here -}
             (_, _, _) -> do
                 let (lbl_end, n2) = getLabel n1
                 (lbl_else, nelse) <- getLabel <$> linear ifnot n2
                 copyInst (Lcondbranch (M.invertTest test) lbl_else)
                    inst <$> (linear ifso =<< addBranch lbl_end nelse)

    M.Iloop body -> do
        let lbl_head = newLabel()
        n1 <- linear (M.next inst) last
        n2 <- linear body (consSimpleInst (Lbranch lbl_head) n1)
        return $ consSimpleInst (Llabel lbl_head) n2

runLinearizer linearizer env = evalState linearizer env

linearizeFunDec fun = FunDec {
    fun_name = M.fun_name fun,
    fun_body = runLinearizer 
        (linear (M.fun_body fun) endInst)
        defaultLinearEnv,
    fun_fast = M.fun_fast fun
    }
