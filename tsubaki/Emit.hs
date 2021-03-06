------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Emit where

import Arch
import Choco
import CmmSyn ( Comp(..), DataItem(..) )
import Linearize ( Inst(..), InstDesc(..), FunDec(..) )
import Mach ( Operation(..), IntOperation(..), Test(..) )
import Outputable
import Panic
import Proc
import Reg

import Control.Monad.State
import Data.Function
import Data.List
import qualified Data.Map as M
import qualified Data.IntMap as I
import System.Time
import Text.Printf

{- The Emission monad -}
type P a = StateT EmitEnv ChocoM a

data EmitEnv = EmitEnv {
  assembly            :: Doc,
  stack_offset        :: Int,

  int_table           :: M.Map Int StaticLocation,
  float_table         :: M.Map Float StaticLocation,
  hsram_next          :: Int,
  static_registers    :: [Int],

  function_name       :: String,
  tailrec_entry_point :: Int,
  is_entry_code       :: Bool,
  nslots              :: Int,
  reg_status          :: I.IntMap Int,
  store_delay         :: Int,
  load_delay          :: Int,
  put_delay           :: Int,
  get_delay           :: Int,
  branch_delay        :: Int,
  hscb_id             :: Maybe Int,

  indent_size         :: Int
  }
  deriving (Show)

initEmitEnv = EmitEnv {
  assembly            = empty,
  stack_offset        = 0,
  int_table           = M.empty,
  float_table         = M.empty,
  hsram_next          = 0,
  static_registers    = [],

  function_name       = "",
  tailrec_entry_point = 0,
  is_entry_code       = True,  -- entry point must comes first
  nslots              = 0,
  reg_status          = I.empty,
  store_delay         = 0,
  load_delay          = 0,
  put_delay           = 0,
  get_delay           = 0,
  branch_delay        = 0,
  hscb_id             = Nothing,

  indent_size         = 0
  }

data FrameDesc = FrameDesc {
  fd_label       :: Int,  -- return address
  fd_frame_size  :: Int,  -- size of stack frame
  fd_live_offset :: [Int] -- offsets/regs of live addresses
  }

reset :: P ()
reset = modify $ \e -> e{
  stack_offset  = 0,
  function_name = "",
  tailrec_entry_point = 0,
  nslots  = 0,
  reg_status = I.empty,
  store_delay = 0
  }

emit :: String -> P ()
emit code = modify $ \e -> e{ 
      assembly = assembly e $$ 
        nest (indent_size e) (text code) 
      }

indent :: Int -> P ()
indent n = modify $ \e -> e{ indent_size = indent_size e + n }

frameSize :: P Int
frameSize = do
  EmitEnv{ stack_offset = x, nslots = n } <- get
  return (x + n)

slotOffset loc = 
  case loc of
    Incoming n -> frameSize >>= return . (+ n)
    Local n    -> do
      offs <- gets stack_offset
      return (offs + n)
    Outgoing n -> return n
  

newLabel = lift $ newUniq

-- FIXME 
-- now select static-locations on FCFS but 
-- it must be selected by frequency of access
newStaticLocInt :: Int -> P StaticLocation
newStaticLocInt n | isUnsignedImm10 n
  = do EmitEnv{ static_registers = sregs, hsram_next = hsram } <- get
       if not (null sregs)
          then do modify $ \e -> e{ static_registers = tail sregs }
                  return $ SReg (head sregs) Nothing
          else if hsram < hsramSize
                  then do modify $ \e -> e{ hsram_next = hsram + 1 }
                          return $ HSRam hsram Nothing
                  else newLabel >>= return . Static
newStaticLocInt n 
  = do EmitEnv{ static_registers = sregs, hsram_next = hsram } <- get
       if not (null sregs)
          then do modify $ \e -> e{ static_registers = tail sregs }
                  l <- newLabel
                  return $ SReg (head sregs) (Just l)
          else if hsram < hsramSize
                  then do modify $ \e -> e{ hsram_next = hsram + 1 }
                          l <- newLabel
                          return $ HSRam hsram (Just l)
                  else newLabel >>= return . Static

newStaticLocFloat :: P StaticLocation
newStaticLocFloat
  = do EmitEnv{ static_registers = sregs, hsram_next = hsram } <- get
       if not (null sregs)
          then do modify $ \e -> e{ static_registers = tail sregs }
                  l <- newLabel
                  return $ SReg (head sregs) (Just l)
          else if hsram < hsramSize
                  then do modify $ \e -> e{ hsram_next = hsram + 1 }
                          l <- newLabel
                          return $ HSRam hsram (Just l)
                  else newLabel >>= return . Static

findIntConst n
  = do EmitEnv{ int_table = table } <- get
       case M.lookup n table of
        Just loc -> return loc
        Nothing  -> 
          do loc <- newStaticLocInt n
             modify $ \e -> e{ int_table = M.insert n loc (int_table e) }
             return loc

findFloatConst n
  = do EmitEnv{ float_table = table } <- get
       l <- case M.lookup n table of
        Just loc -> return loc
        Nothing  -> 
          do loc <- newStaticLocFloat
             modify $ \e -> e{ float_table = M.insert n loc (float_table e) }
             return loc
       return l

{- Nop insertion -}
incDate n = do
  modify $ \e -> e {
    reg_status   = I.map (sub n) (reg_status e),
    store_delay  = sub 1 (store_delay e),
    load_delay   = sub 1 (load_delay e),
    put_delay    = sub 1 (put_delay e),
    get_delay    = sub 1 (get_delay e),
    branch_delay = sub 1 (branch_delay e),
    hscb_id      = Nothing
    }
  where
  sub n x = if x < n then 0 else x - n

setDelay r n = case loc r of
  Register r  -> modify $ \e -> e{ reg_status = I.insert r n (reg_status e) }
  _ -> panic "Emit:setDelay"

pushDelay i = case idesc i of
  Lop op | op `elem` [Imove, Ispill, Ireload]
    -> let src = args i!!0
           dst = result i!!0
        in case (dst, src) of
          (Reg{ loc = Register rd }, Reg{ loc = Stack ss })
            -> do setDelay dst 1
                  modify $ \e -> e{ load_delay = 2 }
          (Reg{ loc = Stack sd }, Reg{ loc = Register rs })
            -> modify $ \e -> e{ store_delay = 2 }
          _ -> return ()
  Lop (Iconst_int n) | not (isUnsignedImm10 n) -> do
    loc <- findIntConst n
    case loc of
      Static _  -> do
        modify $ \e -> e{ load_delay = 2 }
        setDelay (result i!!0) 1
      HSRam n _ -> modify $ \e -> e{ hscb_id = Just n }
      _ -> return ()
  Lop (Iconst_float f) -> do
    loc <- findFloatConst f
    case loc of
      Static _ -> do
        setDelay (result i!!0) 1
        setDelay (result i!!0) 1
      HSRam n _ -> modify $ \e -> e{ hscb_id = Just n }
      _ -> return ()
  Lop (Istore _) -> modify $ \e -> e{ store_delay = 2 }
  Lop (Iload _) -> do
    modify $ \e -> e{ load_delay = 2 }
    setDelay (result i!!0) 1
  Lop op | op `elem` [Ifadd, Ifsub, Ifmul, Ifinv, Isqrt]
    -> setDelay (result i!!0) 2
  Lop Iput -> modify $ \e -> e{ put_delay = 1 }
  Lop Iget -> modify $ \e -> e{ get_delay = 2 }
  Lop (Ihsr n) -> modify $ \e -> e{ hscb_id = Just n }
  Lop (Ihsw n) -> modify $ \e -> e{ hscb_id = Just n }
  Lcondbranch _ _ -> modify $ \e -> e{ branch_delay = 1 }
  _ -> return ()


instReady i = case idesc i of
  Lop op | has_delay_slot op -> do
    s <- gets reg_status
    return $ all (<= 2) (I.elems s)

  Lop Iput -> do
    pd <- gets put_delay
    bd <- gets branch_delay
    ld <- gets load_delay
    c <- argsReady (args i) 0
    return $ c && pd == 0 && bd == 0 && ld == 0

  Lop Iget -> do
    gd <- gets get_delay
    bd <- gets branch_delay
    ld <- gets load_delay
    return $ gd == 0 && bd == 0 && ld == 0

  Lop (Iconst_int n) | not (isUnsignedImm10 n) -> do
    loc <- findIntConst n
    case loc of
      HSRam n _ -> check_hsram n
      _ -> return True

  Lop (Iconst_float f) -> do
    loc <- findFloatConst f
    case loc of
      HSRam n _ -> check_hsram n
      _ -> return True

  Lop (Ihsr n) -> check_hsram n
  Lop (Ihsw n) -> do
    one <- argsReady (args i) 0
    two <- check_hsram n
    return (one && two)

  Lreturn -> do
    s <- gets reg_status
    return $ all (<= 2) (I.elems s)

  Lbranch _ -> do
    s <- gets reg_status
    return $ all (<= 1) (I.elems s)

  Lcondbranch _ _ -> do
    s <- gets reg_status
    return $ all (== 0) (I.elems s)
  _ -> argsReady (args i) 0

  where
  has_delay_slot (Icall_imm _)     = True
  has_delay_slot Icall_ind         = True
  has_delay_slot (Itailcall_imm _) = True
  has_delay_slot Itailcall_ind     = True
  has_delay_slot _                 = False

  check_hsram n1 = do
    ld <- gets load_delay
    bd <- gets branch_delay
    hs <- gets hscb_id
    if ld == 1
      then case hs of
        Just n2 -> return $ n1 /= n2 && bd == 0
        Nothing -> return $ bd == 0
      else return $ bd == 0

regReady r n = do
  s <- gets reg_status
  case loc r of
    Register r  -> return $ I.findWithDefault 0 r s <= n
    _ -> panic "Emit:regReady"

argsReady args n = do
  return . and =<< mapM (\r -> case loc r of
    Register _ -> regReady r n
    _ -> return True
    ) args

{- emissions -}
emitAll i = do
  fname <- gets function_name
  one_inst <- isOneInst i
  case i of
    Inst{ idesc = Lend } -> return ()
    Inst{ next = Inst{ idesc = Lop (Icall_imm _) } }
      | one_inst
      -> do
        emitInst (next i) (Just i)
        emitAll (next (next i))
    Inst{ next = Inst{ idesc = Lbranch _ } }
      | one_inst
      -> do
        emitInst (next i) (Just i)
        emitAll (next (next i))
    Inst{ next = Inst{ idesc = Lop (Itailcall_imm s) } }
      | s == fname && one_inst
      -> do
        emitInst (next i) (Just i)
        emitAll (next (next i))
    Inst{ next = Inst{ idesc = Lop Icall_ind } }
      | one_inst && noInterference (result i) (args (next i))
      -> do
        emitInst (next i) (Just i)
        emitAll (next (next i))
    _ -> do
      r <- instReady i
      if r
        then do
          emitInst i Nothing
          emitAll (next i)
        else do
          emit "nop;"
          incDate 1
          emitAll i
  
emitLoad addr inst =
  case addr of
    Ibased s 0   -> emit $ printf "loadi %s r61 (%s - 4500);" 
                        (emitRes1 inst) (emitSymbol s)
    Ibased s n   -> emit $ printf "loadi %s r61 (%s + %d - 4500);"
                        (emitRes1 inst) (emitSymbol s) n
    Iindexed ofs -> emit $ printf "loadi %s %s %d;"  
                        (emitRes1 inst) (emitArgs inst) ofs

emitStore addr inst =
  case addr of
    Ibased s 0   -> emit $ printf "storei %s r61 (%s - 4500);"
                        (emitArgs inst) (emitSymbol s)
    Ibased s n   -> emit $ printf "storei %s r61 (%s + %d - 4500);"
                        (emitArgs inst) (emitSymbol s) n
    Iindexed ofs -> emit $ printf "storei %s %d;"
                        (emitArgs inst) ofs

emitInst inst dslot = do
  case idesc inst of
    Lend    -> return ()
    Lnop    -> do
      emit "nop;"
      incDate 1

    Lop op | op `elem` [Imove, Ispill, Ireload]
      -> do 
        let src = (args inst) !! 0
        let dst = (result inst) !! 0
        case (loc src, loc dst) of
          (Register rs, Register rd)
            -> emit $ printf "move %s %s;" (emitReg dst) (emitReg src)
          (Register rs, Stack sd)
            -> do stack <- emitStack dst
                  emit $ printf "storei %s %s;" (emitReg src) stack
          (Stack ss, Register rd)
            -> do stack <- emitStack src
                  emit $ printf "loadi %s %s;" (emitReg dst) stack
        incDate 1

    Lop (Iconst_int n)
      -> do 
         if isUnsignedImm10 n
          then emit $ printf "li %s %d;" (emitRes1 inst) n
          else 
            do loc <- findIntConst n 
               case loc of
                Static n  -> emit $ printf "loadi %s r61 (%s - 4500);"
                  (emitRes1 inst) (emitLabel n)
                HSRam n _ -> emit $ printf "hsr %s %d;"
                  (emitRes1 inst) n
                SReg n _  -> emit $ printf "move %s %s;"
                  (emitRes1 inst) (emitReg' n)
         incDate 1

    Lop (Iconst_float f)
      -> do loc <- findFloatConst f
            case loc of
              Static n  -> emit $ printf "loadi %s r61 (%s - 4500);"
                (emitRes1 inst) (emitLabel n)
              HSRam n _ -> emit $ printf "hsr %s %d;"
                (emitRes1 inst) n
              SReg n _  -> emit $ printf "move %s %s;"
                (emitRes1 inst) (emitReg' n)
            incDate 1

    Lop (Iconst_symbol s)
      -> do 
        emit $ printf "addiu %s r61 (%s - 4500);" (emitRes1 inst) (emitSymbol s)
        incDate 1

    Lop Icall_ind 
      -> do emit $ printf "call %s;" (emitArgs inst)
            fillDelaySlot dslot
            incDate 1

    Lop (Icall_imm s)
      -> do emit $ printf "calli %s;" (emitSymbol s)
            fillDelaySlot dslot
            incDate 1

    Lop Itailcall_ind
      -> do emit $ printf "jump %s;" (emitArgs inst)
            n <- frameSize
            if n > 0
              then emit $ printf "addiu r1 r1 %d;" n
              else emit $ printf "nop;"
            incDate 1

    Lop (Itailcall_imm s)
      -> do f <- gets function_name
            if s == f
              then do n <- gets tailrec_entry_point
                      emit $ printf "jumpi %s;" (emitLabel n)
                      fillDelaySlot dslot
              else do emit $ printf "jumpi %s;" (emitSymbol s)
                      n <- frameSize
                      if n > 0
                        then emit $ printf "addiu r1 r1 %d;" n
                        else emit $ printf "nop;" 
            incDate 1

    Lop (Istackoffset n)
      -> do if n > 0 
              then emit $ printf "subiu r1 r1 %d;" n
              else emit $ printf "addiu r1 r1 %d;" (-n)
            modify $ \e -> e{ stack_offset = stack_offset e + n }
            incDate 1

    Lop (Iload addr) -> do
      emitLoad addr inst
      incDate 1

    Lop (Istore addr) -> do
      emitStore addr inst
      incDate 1

    Lop (Ialloc n) 
      -> do emit $ printf "move %s r62;" (emitRes1 inst)
            emit $ printf "addiu r62 r62 %d;" n
            incDate 2
    Lop (Iintop op)
      -> do emit $ printf "%s %s %s;"
                    (nameIntop op) (emitRes1 inst) (emitArgs inst)
            incDate 1
    Lop (Iintop_imm op imm)
      -> do emit $ printf "%s %s %s %d;"
                    (nameIntopImm op) (emitRes1 inst) (emitArgs inst) imm
            incDate 1
    Lop op | op `elem` [Iftoi, Iitof,  Ifneg, Ifabs, Isqrt, Ifinv]
      -> do emit $ printf "%s %s %s;"
                    (nameFloatop1 op) (emitRes1 inst) (emitArgs inst)
            incDate 1
    Lop op | op `elem` [Ifadd, Ifsub, Ifmul]
      -> do emit $ printf "%s %s %s;" 
                    (nameFloatop2 op) (emitRes1 inst) (emitArgs inst)
            incDate 1

    Lop Iput -> do
      emit $ printf "put %s;" (emitArgs inst)
      incDate 1
    Lop Iget -> do
      emit $ printf "get %s;" (emitArgs inst)
      incDate 1

    Lop (Ihsw n) -> do
      emit $ printf "hsw %s %d;" (emitArgs inst) n
      incDate 1
    Lop (Ihsr n) -> do
      emit $ printf "hsr %s %d;" (emitRes1 inst) n
      incDate 1

    Lreturn -> do
      r <- gets is_entry_code
      when (not r) $ do
         emit "ret;"
         n <- frameSize
         if n == 0 
          then emit "nop;"
          else emit $ printf "addiu r1 r1 %d;" n
         incDate 1
    
    Llabel l -> do 
      indent (-4) 
      emit $ printf "%s:" (emitLabel l)
      indent 4

    Lbranch l -> 
      do emit $ printf "jumpi %s;" (emitLabel l)
         fillDelaySlot dslot
         incDate 1

    Lcondbranch test l
      -> do
        case test of
          Itruetest  -> emit $ printf "bgi %s 0 %s;"
                            (emitArgs inst) (emitLabel l)
          Ifalsetest -> emit $ printf "bli %s 0 %s;"
                            (emitArgs inst) (emitLabel l)
          Iinttest cmp 
            -> emit $ printf "%s %s %s;"
                      (nameIntComp cmp) (emitArgs inst) (emitLabel l)
          Iinttest_imm cmp n | isSignedImm6 n
            -> emit $ printf "%s %s %d %s;"
                      (nameIntCompImm cmp) (emitArgs inst) n (emitLabel l)
          Iinttest_imm cmp n -> panic "Emit:emitInst:Lcondbranch"
          Ifloattest cmp neg
            -> emit $ printf "%s %s %s;"
                      (nameFloatComp cmp neg) (emitArgs inst) (emitLabel l)
        incDate 1
  pushDelay inst

fillDelaySlot code = do
  indent 4
  case code of
    Nothing -> emit "nop;"
    Just i  -> emitInst i Nothing
  indent (-4)

isOneInst i = do
  c1 <- argsReady (args i) 1
  c2 <- case idesc i of
    Lop op -> case op of
      _ | op `elem` [Imove, Ispill, Ireload]
        -> case (args i!!0, result i!!0) of
            (Reg{ loc = Register _ }, Reg{ loc = Register _ }) 
              -> return True
            (Reg{ loc = Register _ }, Reg{ loc = Stack _ })
              -> return True
            _ -> return False
      _ | op `elem` [Ifneg, Ifabs] -> return True
      Iconst_int n | isUnsignedImm10 n -> return True
      Iconst_int n -> do
        loc <- findIntConst n
        case loc of
          Static _ -> return False
          _ -> return True
      Iconst_float f -> do
        loc <- findFloatConst f
        case loc of
          Static _ -> return False
          _ -> return True
      Ihsr _ -> return True
      Ihsw _ -> return True
      Istackoffset _  -> return True
      Iintop op       -> return True
      Iintop_imm op _ -> return True
      Istore _ -> return True
      _ -> return False
    _ -> return False
  return (c1 && c2)

noInterference res arg = or [loc x == loc y | x <- res, y <- arg]

emitInitConstData = do
  EmitEnv{ int_table = itab, float_table = ftab } <- get
  emit "/* initialize constants */"
  -- initialize global registers
  let isregs = filter isSReg (M.toList itab)
  mapM ( \(n, SReg r init) -> 
    case init of
      Nothing -> emit $ printf "li %s %d;" (emitReg' r) n
      Just i  -> emit $ printf "loadi %s r61 (%s - 4500);" (emitReg' r) (emitLabel i)
      ) (sortBy (compare `on` snd) isregs)
  let fsregs = filter isSReg (M.toList ftab)
  mapM ( \(_, SReg r (Just i)) ->
    emit $ printf "loadi %s r61 (%s - 4500);" (emitReg' r) (emitLabel i)
      ) (sortBy (compare `on` snd) fsregs)

  -- initialize high-speed ram
  let ihsram = filter isHSRam (M.toList itab)
  mapM ( \(n, HSRam r init) ->
    case init of
      Nothing
        -> do emit $ printf "li r63 %d;" n
              emit $ printf "hsw r63 %d;" r
      Just i
        -> do emit $ printf "loadi r63 r61 (%s - 4500);" (emitLabel i)
              emit "nop;"
              emit $ printf "hsw r63 %d;" r
        ) (sortBy (compare `on` snd) ihsram)
  let fhsram = filter isHSRam (M.toList ftab)
  mapM ( \(_, HSRam r (Just i)) ->
        do emit $ printf "loadi r63 r61 (%s - 4500);" (emitLabel i)
           emit "nop;"
           emit $ printf "hsw r63 %d;" r
        ) (sortBy (compare `on` snd) fhsram)

  where

  isSReg (_, SReg _ _) = True
  isSReg _             = False
  isHSRam (_, HSRam _ _) = True
  isHSRam _              = False

emitConstData = do
  emit "/* constants */"
  indent (-4)
  EmitEnv{ int_table = itab, float_table = ftab } <- get
  mapM (\(n, loc) 
    -> case loc of
        Static l 
          -> emit $ printf "%s:  .int %d;" (emitLabel l) n
        SReg _ (Just l)
          -> emit $ printf "%s:  .int %d;" (emitLabel l) n
        HSRam _ (Just l)
          -> emit $ printf "%s:  .int %d;" (emitLabel l) n
        _ -> return ()
    ) (sortBy (compare `on` snd) $ M.toList itab)

  mapM (\(f, loc)
    -> case loc of
        Static l
          -> emit $ printf "%s:  .float %f;" (emitLabel l) f
        SReg _ (Just l)
          -> emit $ printf "%s:  .float %f;" (emitLabel l) f
        HSRam _ (Just l)
          -> emit $ printf "%s:  .float %f;" (emitLabel l) f
    ) (sortBy (compare `on` snd) $ M.toList ftab)
  indent 4

emitReg r =
  case loc r of
    Register i  -> 'r':show i
    _ -> panic "emitReg"
emitReg' n = 'r':show n

emitStack :: Reg -> P String
emitStack r =
  case loc r of
    Stack s -> do ofs <- slotOffset s
                  return $ printf "r1 %d" ofs
    _ -> panic "emitStack"

labelPrefix = "L."
emitLabel num = labelPrefix ++ show num
emitSymbol str = str

emitRes1 inst  = emitReg (head (result inst))
emitArgs inst  = concat $ intersperse " " $ map emitReg (args inst)

asmHead = do
  date <- lift $ runIO getClockTime
  emit $ printf "/* Generated by choco in %s*/" (show date)
  emit "/* need two nops first */"
  emit "nop;"
  emit "nop;"
  emit "li r1 768;"
  emit "slliu r1 r1 4;  /* initialize stack pointer */"
  emit "move r62 r1;    /* initialize heap pointer */"
  emit "li r61 1023;"
  emit "slliu r61 r61 2;"
  emit "addiu r61 r61 408;"
  emit "calli lib_const_load;"
  emit "addiu r60 r61 500;"
  emitInitConstData

asmTail = do
  emitConstData
  emit "/* Code end */"

emitItems items =
  case items of
    [] -> return ()
    Cglobal_symbol lbl : rem -> do
      indent (-4)
      emit $ printf "%s:" lbl
      indent 4
      emitItems rem
    Cint n : rem  -> do
      emit $ printf ".int %d;" n
      emitItems rem
    Cfloat f : rem -> do
      emit $ printf ".float %f;" f
      emitItems rem
    Cskip  n : rem -> do
      emit $ printf ".skip %d;" n
      emitItems rem
    Cstatic_array s n : rem -> do
      let (elem, rem') = splitAt s rem
      let array = concat $ replicate n $ concatMap transl elem
      emit array
      emitItems rem'
    _ -> panic "Emit:emitItems"
    where
    transl (Cint n)  = printf ".int %d;" n
    transl (Cfloat f) = printf ".float %f;" f
    transl _ = panic "Emit:emitItems:transl"

fundecl (fd, nslots) = do 
  id <- lift$newUniq
  modify $ \e -> e{ 
    function_name = fun_name fd,
    tailrec_entry_point = id,
    nslots = nslots
    }
  indent (-4)
  emit $ printf "%s:" (fun_name fd)
  indent 4
  n <- frameSize
  when (n > 0) $
    emit $ printf "subiu r1 r1 %d;" n
  indent (-4)
  emit $ printf "%s:" (emitLabel id)
  indent 4
  emitAll (fun_body fd)

  r <- gets is_entry_code
  when r $ do
    id <- lift$newUniq
    indent (-4)
    emit $ printf "%s:" (emitLabel id)
    indent 4
    emit $ printf "halt;"
    emit $ printf "jumpi %s;" (emitLabel id)
    emit $ printf "nop;"
    modify $ \e -> e{ is_entry_code = False }
  reset

fundecls (hsram, greg, itab, ftab) datas fds = evalStateT (do
  indent 4
  mapM fundecl fds
  body <- gets assembly
  modify $ \e -> e{ assembly = empty }
  asmHead 
  modify $ \e -> e{ assembly = assembly e $$ body }
  emitItems $ concat datas
  asmTail
  gets assembly
  ) initEmitEnv{ 
      hsram_next = hsram, 
      static_registers = [greg .. globalRegEnd - 1],
      int_table = itab,
      float_table = ftab
      }

{- utilities -}
nameIntop op = case op of
  Iadd  -> "adds"
  Isub  -> "subs"
  Imul  -> "mul"
  Isra  -> "sra"
  Isll  -> "sll"

nameIntopImm op = case op of
  Iadd  -> "addiu"
  Isub  -> "subiu"
  Imul  -> "muliu"

nameFloatop1 op = case op of
  Ifabs -> "fabs"
  Ifneg -> "fneg"
  Isqrt -> "fsqrt"
  Ifinv -> "finv"
  Iftoi -> "ftoi"
  Iitof -> "itof"

nameFloatop2 op = case op of
  Ifadd -> "fadd"
  Ifsub -> "fsub"
  Ifmul -> "fmul"

nameIntComp cmp = case cmp of
  Ceq -> "be"  
  Cne -> "bne"
  Cle -> "ble" 
  Cge -> "bge"
  Clt -> "bl"  
  Cgt -> "bg"

nameIntCompImm cmp = case cmp of
  Ceq -> "bei"  
  Cne -> "bnei"
  Cle -> "blei" 
  Cge -> "bgei"
  Clt -> "bli"  
  Cgt -> "bgi"

nameFloatComp cmp neg =
  case cmp of
  Ceq -> if neg then "bnef" else "bef"
  Cne -> if neg then "bef"  else "bnef"
  Cle -> if neg then "bgf"  else "blef"
  Cge -> if neg then "blf"  else "bgef"
  Clt -> if neg then "bgef" else "blf"
  Cgt -> if neg then "blef" else "bgf"

