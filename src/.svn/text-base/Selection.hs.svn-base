------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Selection where

{- Selection of pseudo-instructions, assignment of pseudo-registers,
   sequentialization -}

import Arch
import Choco
import CmmSyn
import Mach
import Outputable
import Panic
import Proc
import Reg
import RegM
import SrcLoc
import Var

import Control.Monad.State
import qualified Data.Map as M

type P a = StateT Env ChocoM a
data Env = Env {
  current_function_name :: String,
  instr_seq             :: Inst,
  int_table             :: M.Map Int StaticLocation,
  float_table           :: M.Map Float StaticLocation
  }

initEnv = Env {
  current_function_name = "",
  instr_seq = emptyInst,
  int_table = M.empty,
  float_table = M.empty
  }

insert desc res arg
  = modify $ \e -> e{ instr_seq = consInst desc res arg (instr_seq e) }

extract :: P Inst
extract = do 
  inst <- gets instr_seq
  return $ iter endInst inst
  where
  iter res i 
    | idesc i == Iend = res
    | otherwise = iter (consInst (idesc i) (result i) (args i) res) (next i)

insertMove dst src
  | stamp src /= stamp dst
    = insert (Iop Imove) [dst] [src]
  | otherwise = return()

insertMoves :: [Reg] -> [Reg] -> P ()
insertMoves = zipWithM_ insertMove 

insertMoveArgs loc arg stacksize = do
  when (stacksize /= 0) $
    insert (Iop (Istackoffset stacksize)) [] []
  insertMoves loc arg
  
insertMoveResults res loc stacksize = do
  when (stacksize /= 0) $
    insert (Iop (Istackoffset (-stacksize))) [] []
  insertMoves res loc

insertOp op rd rs = do
  insert (Iop op) rd rs
  return rd

isSimpleExpr e = case e of
  Cconst_int _  -> True
  Cconst_float _ -> True
  Cconst_symbol _ -> True
  Cvar _ -> True
  Ctuple es -> all isSimpleExpr es
  Clet id e1 e2 -> isSimpleExpr e1 && isSimpleExpr e2
  Cseq e1 e2 -> isSimpleExpr e1 && isSimpleExpr e2
  Cop op args -> case op of
    Capply _  -> False
    Cextcall _ _ -> False
    Calloc  -> False
    Cstore  -> False
    Chsr _  -> True
    Cregr _ -> True
    Cfill _ -> False
    Cput -> False
    _ -> all isSimpleExpr args
  _ -> False

operHasResult op = case op of
  Capply p     -> p
  Cextcall s p -> p
  Cstore       -> False
  Cregw _      -> False
  Chsw _       -> False
  _            -> True

selectOperation op args = case (op, args) of
  (Capply _, Cconst_symbol s : rem) -> (Icall_imm s, rem)
  (Capply _, _) -> (Icall_ind, args)
  (Cextcall s _, _) -> (Icall_imm s, args)
  (Cload, [arg]) ->
    let (addr, eloc) = selectAddressing arg in
    (Iload addr, [eloc])
  (Cstore, [arg1, arg2]) ->
    let (addr, eloc) = selectAddressing arg1 in
    (Istore addr, [arg2, eloc])
  (Calloc, _) -> (Ialloc 0, args)
  (Caddi, _) -> selectArithComm Iadd args
  (Csubi, _) -> selectArith Isub args
  (Cmuli, _) -> selectArithComm Imul args
  (Ccompi cmp, _) -> panic "Selection:selectOperation:Ccompi"
  (Cnegf, _)  -> (Ifneg, args)
  (Cabsf, _)  -> (Ifabs, args)
  (Caddf, _)  -> (Ifadd, args)
  (Csubf, _)  -> (Ifsub, args)
  (Cmulf, _)  -> (Ifmul, args)
  (Csqrt, _)  -> (Isqrt, args)
  (Cinv, _)  -> (Ifinv, args)
  (Cftoi, _)  -> (Iftoi, args)
  (Citof, _)  -> (Iitof, args)
  (Cput, _)   -> (Iput, args)
  (Chsw n, _) -> (Ihsw n, args)
  (Chsr n, _) -> (Ihsr n, args)
  _ -> panic $ "Select:selectOperation " ++ show op

selectCondition test = case test of
  Cop (Ccompi cmp) [arg1, Cconst_int n] | isSignedImm6 n
    -> (Iinttest_imm cmp n, arg1)
  Cop (Ccompi cmp) [Cconst_int n, arg2] | isSignedImm6 n
    -> (Iinttest_imm (swapComp cmp) n, arg2)
  Cop (Ccompi cmp) args
    -> (Iinttest cmp, Ctuple args)
  Cop (Ccompf cmp) args
    -> (Ifloattest cmp False, Ctuple args)
  arg -> (Itruetest, arg)

selectArith op args = case args of
  [arg, Cconst_int n] | isUnsignedImm10 n
    -> (Iintop_imm op n, [arg])
  args -> (Iintop op, args)

selectArithComm op args = case args of
  [arg, Cconst_int n] | isUnsignedImm10 n
    -> (Iintop_imm op n, [arg])
  [Cconst_int n, arg] | isUnsignedImm10 n
    -> (Iintop_imm op n, [arg])
  args -> (Iintop op, args)

selectAddressing arg = case arg of
  Cconst_symbol s -> (Ibased s 0, Ctuple [])
  Cop Caddi [Cconst_symbol s, Cconst_int n] -> (Ibased s n, Ctuple [])
  Cop Caddi [arg, Cconst_int n] -> (Iindexed n, arg)
  Cop Caddi [arg1, Cop Caddi [arg2, Cconst_int n]] ->
    (Iindexed n, Cop Caddi [arg1, arg2])
  _ -> (Iindexed 0, arg)

offsetAddressing addr delta = case addr of 
  Ibased s n  -> Ibased s (n + delta)
  Iindexed n  -> Iindexed (n + delta)
                                           
sizeExpr env exp = size M.empty exp
  where
  size lenv e = case e of
    Cconst_int _    -> 1
    Cconst_float _  -> 1
    Cconst_symbol _ -> 1
    Cvar id -> case M.lookup id lenv of
      Just s  -> s
      Nothing -> case M.lookup id env of
        Just regs -> length regs
        Nothing   -> panic "Selection:sizeExpr"
    Ctuple el -> sum $ map (size lenv) el
    Cop op args -> 1
    Clet id e1 e2 -> size (M.insert id (size lenv e1) lenv) e2
    Cseq e1 e2 -> size lenv e2
    _ -> panic "Selection:sizeExpr"

{- Add the instructions for the given expression -}
emitExpr env exp = case exp of
  Cconst_int n -> do
    r <- lift$createReg 
    return . Just =<< insertOp (Iconst_int n) [r] []

  Cconst_float f -> panic "Selection:emitExpr"

  Cconst_symbol s -> do
    r <- lift$createReg
    return . Just =<< insertOp (Iconst_symbol s) [r] []

  Cvar v -> case M.lookup v env of
    Just rs -> return (Just rs)
    Nothing -> lift$simpleError (text "Selection:emitExpr : unbound variable" <+> text v)

  Clet v e1 e2 -> do
    r <- emitExpr env e1
    case r of
      Just r1 -> do 
        env' <- bindLet env v r1
        emitExpr env' e2
      Nothing -> return Nothing

  Cassign v e -> case M.lookup v env of
    Just rd -> do
        r <- emitExpr env e
        case r of
          Just rs -> do 
            insertMoves rd rs
            return $ Just []
          Nothing -> return Nothing
    Nothing -> lift$simpleError (text "Selection:emitExpr : unbound variable" <+> text v)

  Ctuple [] -> return (Just [])

  Ctuple es -> do
    r <- emitPartsList env es
    case r of
      Nothing -> return Nothing
      Just (simple_list, ext_env) -> 
        return . Just =<< emitTuple ext_env simple_list

  Cop (Ccompf cmp) args ->
    emitExpr env (Ccond exp (Cconst_int 1) (Cconst_int (-1)))

  Cop (Cregw n) [arg] -> do
    r <- emitExpr env arg
    case r of
      Nothing -> return Nothing
      Just r1 -> do
        r2 <- lift$physReg n
        insertMoves [r2] r1
        return $ Just []

  Cop (Cregr n) [] -> do
    r <- lift$physReg n
    return $ Just [r]

  Cop (Chsr n) [] -> do
    rd <- lift$createReg
    return . Just =<< insertOp (Ihsr n) [rd] []

  Cop (Chsw n) [e] -> do
    e' <- emitExpr env e
    case e' of
      Nothing -> return Nothing
      Just r1 -> return . Just =<< insertOp (Ihsw n) [] r1

  Cop op args -> do
    r <- emitPartsList env args
    case r of
      Nothing -> return Nothing
      Just (simple_args, new_env) -> do
        let (new_op, new_args) = selectOperation op simple_args
        case new_op of
          Icall_ind -> do
            lift$modify$ \e -> e{ contains_calls = True }
            r1 <- emitTuple new_env new_args
            rd <- lift$createRegv (operHasResult op)
            loc_res <- lift$locResults rd
            let rarg = tail r1
            (loc_arg, stack_ofs) <- lift$locArguments rarg
            insertMoveArgs loc_arg rarg stack_ofs
            insert (Iop Icall_ind) loc_res (head r1 : loc_arg)
            insertMoveResults loc_res rd stack_ofs
            return $ Just rd

          Icall_imm lbl -> do
            lift$modify$ \e -> e{ contains_calls = True }
            r1 <- emitTuple new_env new_args
            rd <- lift$createRegv (operHasResult op)
            (loc_arg, stack_ofs) <- lift$locArguments r1
            loc_res <- lift$locResults rd
            insertMoveArgs loc_arg r1 stack_ofs
            insert (Iop (Icall_imm lbl)) loc_res loc_arg
            insertMoveResults rd loc_res stack_ofs
            return $ Just rd

          Ialloc _ -> do
            rd <- lift$createReg
            let size = sizeExpr new_env (Ctuple new_args)
            insert (Iop (Ialloc size)) [rd] []
            emitStores new_env new_args [rd]
            return $ Just [rd]

          op1 -> do
            r1 <- emitTuple new_env new_args
            rd <- lift$createRegv (operHasResult op)
            return . Just =<< insertOp op1 rd r1

  Cseq e1 e2 -> do
    r <- emitExpr env e1
    case r of
      Nothing -> return Nothing
      Just r1 -> emitExpr env e2

  c@(Ccond test ifso ifnot) -> do
    let (cond, earg) = selectCondition test
    r <- emitExpr env earg
    case r of
      Nothing -> return Nothing
      Just rarg -> do
        (rif, sif)        <- emitSequence env ifso
        (relse, selse)    <- emitSequence env ifnot
        (r, sif', selse') <- joinSequence rif sif relse selse
        sif_code <- lift$evalStateT extract sif'
        selse_code <- lift$evalStateT extract selse'
        insert (Icond cond sif_code selse_code) [] rarg
        return r

emitParts env exp 
  | isSimpleExpr exp = return $ Just (exp, env)
  | otherwise = do
    r <- emitExpr env exp
    case r of
      Nothing -> return Nothing
      Just rs -> case length rs of
        0 -> return $ Just (Ctuple [], env)
        1 -> do -- normal case
          id <- lift$mkIdent "bind"
          let r0 = head rs
          if name r0 == ""
            then return $ Just (Cvar id, M.insert id rs env)
            else do
              v <- lift$createReg
              insertMove v r0
              return $ Just (Cvar id, M.insert id [v] env)
        _ -> lift$simpleError (text "emitParts" <+> ppr exp)

emitPartsList env exp_list = case exp_list of
  []  -> return $ Just ([], env)
  (e:rem) -> do
    r <- emitPartsList env rem
    case r of
      Nothing -> return Nothing
      Just (new_rem, new_env) -> do
        r <- emitParts new_env e
        case r of
          Nothing -> return Nothing
          Just (new_e, fin_env) -> return $ Just (new_e : new_rem, fin_env)

emitTuple env exp_list = iter exp_list
  where
  iter [] = return []
  iter (e:rem) = do
    loc_rem <- iter rem
    r <- emitExpr env e
    case r of
      Nothing -> panic "Selection:emitTuple"
      Just loc_e  -> return $ loc_e ++ loc_rem

emitStores env dat regs_addr = iter dat (Iindexed 0)
  where
  iter [] _ = return ()
  iter (e:rem) a = do
    r <- emitExpr env e
    case r of
      Nothing -> panic "Selection:emitStores"
      Just regs -> do
        a' <- foldM (\a1 r -> do
          insert (Iop (Istore a1)) [] (r:regs_addr)
          return $ offsetAddressing a1 1
          ) a regs
        iter rem a'

emitSequence env e = do
  Env{ int_table = itab, float_table = ftab } <- get
  (r, env') <- lift$runStateT (emitExpr env e) 
                      initEnv{ int_table = itab, float_table = ftab }
  return (r, env')

{- tail position -}
emitTail env exp = case exp of
  Clet v e1 e2 -> do
    r <- emitExpr env e1
    case r of
      Nothing -> return ()
      Just r1 -> do
        env' <- bindLet env v r1
        emitTail env' e2

  Cop op@(Capply p) args -> do
    r <- emitPartsList env args
    case r of
      Nothing -> return ()
      Just (simple_args, new_env) -> do
        let (new_op, new_args) = selectOperation op simple_args
        case new_op of
          Icall_ind -> do
            r1 <- emitTuple new_env new_args
            let rarg = tail r1
            (loc_arg, stack_ofs) <- lift$locArguments rarg
            if stack_ofs == 0
              then do
                insertMoves loc_arg rarg
                insert (Iop Itailcall_ind) [] (head r1 : loc_arg)
              else do
                lift$modify$ \e -> e{ contains_calls = True }
                rd <- lift$createRegv p
                loc_res <- lift$locResults rd
                insertMoveArgs loc_arg rarg stack_ofs
                insert (Iop Icall_ind) loc_res (head r1 : loc_arg)
                insert (Iop (Istackoffset (-stack_ofs))) [] []
                insert Ireturn [] loc_res

          Icall_imm lbl -> do
            r1 <- emitTuple new_env new_args
            (loc_arg, stack_ofs) <- lift$locArguments r1
            current_fun <- gets current_function_name
            if stack_ofs == 0
              then do
                insertMoves loc_arg r1
                insert (Iop (Itailcall_imm lbl)) [] loc_arg
              else if lbl == current_fun
                then do
                  loc_arg' <- lift$locParameters r1
                  insertMoves loc_arg' r1
                  insert (Iop (Itailcall_imm lbl)) [] loc_arg'
                else do
                  lift$modify$ \e -> e{ contains_calls = True }
                  rd <- lift$createRegv p
                  loc_res <- lift$locResults rd
                  insertMoveArgs loc_arg r1 stack_ofs
                  insert (Iop (Icall_imm lbl)) loc_res loc_arg
                  insert (Iop (Istackoffset (-stack_ofs))) [] []
                  insert Ireturn [] loc_res
          _ -> panic "Selection:emitTail"

  Cseq e1 e2 -> do
    r <- emitExpr env e1
    case r of
      Nothing -> return ()
      Just r1 -> emitTail env e2

  Ccond econd eif eelse -> do
    let (cond, earg) = selectCondition econd
    r <- emitExpr env earg
    case r of
      Nothing -> return ()
      Just rarg -> do
        eif' <- emitTailSequence env eif
        eelse' <- emitTailSequence env eelse
        insert (Icond cond eif' eelse') [] rarg
  _ -> emitReturn env exp

emitReturn env exp = do
  r <- emitExpr env exp
  case r of
    Nothing -> return ()
    Just r1 -> do
      loc <- lift$locResults r1
      insertMoves loc r1
      insert Ireturn [] loc

emitTailSequence env exp = do
  Env{ int_table = itab, float_table = ftab } <- get
  lift$evalStateT (emitTail env exp >> extract) 
    initEnv{ int_table = itab, float_table = ftab }

bindLet env v rs 
  | all (null . name) rs
    = do let rs' = nameRegs v rs
         return $ M.insert v rs' env
  | otherwise
    = do rs2 <- replicateM (length rs) (lift createReg)
         let rs2' = nameRegs v rs2
         insertMoves rs2' rs 
         return $ M.insert v rs2' env
         

nameRegs id [r] = [r{ name = id }]
nameRegs id rs  = zipWith (\r i -> r{ name = id ++ "#" ++ show i } )
                    rs [0..]

{- join two instruction sequences, make sure they return their results in the same registers -}

joinSequence opt_r1 seq1 opt_r2 seq2 = case (opt_r1, opt_r2) of
  (Nothing, _) -> return (opt_r2, seq1, seq2)
  (_, Nothing) -> return (opt_r1, seq1, seq2)
  (Just r1, Just r2) -> do
    when (length r1 /= length r2) $ do
      panic$ "Selection:join"
    (regs, seq1', seq2') <- foldM (\(regs, env1, env2) (reg1, reg2) -> do
      if name reg1 == ""
        then do
          env2' <- lift$execStateT (insertMove reg1 reg2) env2
          return (reg1:regs, env1, env2')
        else if name reg2 == ""
          then do
            env1' <- lift$execStateT (insertMove reg2 reg1) env1
            return (reg2:regs, env1', env2)
          else do
            r <- lift$createReg
            env1' <- lift$execStateT (insertMove r reg1) env1
            env2' <- lift$execStateT (insertMove r reg2) env2
            return (r:regs, env1', env2')
        ) ([], seq1, seq2) (zip r1 r2)
    return (Just regs, seq1', seq2')

{- Interface -}
emitFundecl :: CmmSyn.FunDec -> P Mach.FunDec
emitFundecl f = do
  lift$modify$ \e -> e{ contains_calls = False } 
  modify$ \e -> e{ current_function_name = funName f }
  rargs <- mapM (\(id, ty) -> do 
    r <- lift$createReg 
    return [r{ name = id }]
    ) (funArgs f)
  loc_args <- lift$locParameters (concat rargs)
  let env = M.fromList $ zip (map fst (funArgs f)) rargs
  insertMoves (concat rargs) loc_args
  emitTail env (funBody f)
  body <- extract
  return Mach.FunDec {
    fun_name  = funName f,
    fun_args  = loc_args,
    fun_body  = body
    }

fundecl f itab ftab = 
  evalStateT (emitFundecl f) 
    initEnv{
      int_table   = itab,
      float_table = ftab
      }
