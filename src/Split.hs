------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Split where

import Choco
import Mach
import Outputable
import Reg
import RegM

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S

{- Renaming of registers at reload points to split live ranges -}

type P a = StateT Env ChocoM a
type Subst = M.Map Reg Reg

data Env = Env {
  equiv_classes :: Subst
  }

initEnv = Env {
  equiv_classes = M.empty
  }

substReg :: Subst -> Reg -> Reg
substReg sub r = M.findWithDefault r r sub

substRegs :: Maybe Subst -> [Reg] -> [Reg]
substRegs sub rv =
  case sub of
    Nothing -> rv
    Just s  -> map (substReg s) rv

repres_reg :: Reg -> P Reg
repres_reg r = do
  e <- gets equiv_classes
  case M.lookup r e of
    Just r' -> repres_reg r'
    Nothing -> return r

repres_regs :: [Reg] -> P [Reg]
repres_regs rv = mapM repres_reg rv

identify :: Reg -> Reg -> P ()
identify r1 r2 = do
  repres1 <- repres_reg r1
  repres2 <- repres_reg r2
  when (stamp repres1 /= stamp repres2) $ 
    modify $ \e -> e{
      equiv_classes = M.insert repres1 repres2 (equiv_classes e)
      }

identifySub :: Subst -> Subst -> Reg -> P ()
identifySub sub1 sub2 reg = do 
  case M.lookup reg sub1 of
    Just r1 -> case M.lookup reg sub2 of
      Just r2 -> identify r1 r2
      Nothing -> identify r1 reg
    Nothing -> case M.lookup reg sub2 of
      Just r2 -> identify r2 reg
      Nothing -> return ()

mergeSubsts :: Maybe Subst -> Maybe Subst -> Inst -> P (Maybe Subst)
mergeSubsts sub1 sub2 i =
  case (sub1, sub2) of
    (Nothing, Nothing)  -> return Nothing
    (Just _, Nothing)   -> return sub1
    (Nothing, Just _)   -> return sub2
    (Just s1, Just s2)  -> do
      mapM_ (identifySub s1 s2) $
        S.toList (S.union (live i) (S.fromList $ args i))
      return sub1

rename :: Inst -> Maybe Subst -> P (Inst, Maybe Subst)
rename i subst =
  case idesc i of
    Iend  -> return (i, subst)
    Ireturn -> return
      (consInst (idesc i) [] (substRegs subst (args i)) (next i), Nothing)
    Iop Itailcall_ind -> return
      (consInst (idesc i) [] (substRegs subst (args i)) (next i), Nothing)
    Iop (Itailcall_imm _) -> return
      (consInst (idesc i) [] (substRegs subst (args i)) (next i), Nothing)
    Iop Ireload | loc (result i !! 0) == Unknown -> 
      case subst of
        Nothing -> rename (next i) subst
        Just s -> do
          let older = result i !! 0
          newer <- lift $ copyReg older
          (new_next, sub_next) <-
            rename (next i) (Just $ M.insert older newer s)
          return (consInst (idesc i) [newer] (args i) new_next, sub_next)
    Iop _ -> do
      (new_next, sub_next) <- rename (next i) subst
      return (consInst (idesc i) 
                (substRegs subst (result i))
                (substRegs subst (args i))
                new_next,
              sub_next)
    Icond tst ifso ifnot -> do
      (new_ifso, sub_ifso) <- rename ifso subst
      (new_ifnot, sub_ifnot) <- rename ifnot subst
      (new_next, sub_next) <-
        rename (next i)  =<< mergeSubsts sub_ifso sub_ifnot (next i)
      return (consInst (Icond tst new_ifso new_ifnot)
                []
                (substRegs subst (args i))
                new_next,
              sub_next)

set_repres i =
  instIter (\i -> do 
    new_args   <- repres_regs (args i)
    new_result <- repres_regs (result i)
    return i{ args = new_args, result = new_result }
    ) i

fundecl :: FunDec -> ChocoM FunDec
fundecl f = evalStateT (do
  (new_body, sub_body) <- rename (fun_body f) (Just M.empty)
  new_args  <- repres_regs (fun_args f)
  new_body' <- set_repres new_body
  return f{ fun_args = new_args, fun_body = new_body' }
  ) initEnv
