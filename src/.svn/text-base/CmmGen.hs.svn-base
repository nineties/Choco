------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module CmmGen (
  translCmm
  ) where

import Arch
import Choco
import CLamSyn
import CmmSyn
import Const
import LamSyn
import Outputable
import Panic
import Proc
import Reg
import Types
import Var

import Control.Monad.State
import Data.Function
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

{- Rough approximation of costs for each global variables -}
data CostEnv = CostEnv {
  var_cost    :: M.Map Var (Int, Bool),
  int_cost    :: M.Map Int Int,
  float_cost  :: M.Map Float Int,
  bias        :: Int
  }

initCostEnv = CostEnv {
  var_cost   = M.empty,
  int_cost   = M.empty,
  float_cost = M.empty,
  bias       = 0
  }

getVCost v = do
  table <- gets var_cost
  return $ fst $ M.findWithDefault (0, False) v table

getICost i = do
  table <- gets int_cost
  return $ M.findWithDefault 0 i table

getFCost f = do
  table <- gets float_cost
  return $ M.findWithDefault 0 f table

incICost i n = do
  table <- gets int_cost
  b     <- gets bias
  case M.lookup i table of
    Just c  -> modify $ \e -> e{ int_cost = M.insert i (c + n + b) table }
    Nothing -> modify $ \e -> e{ int_cost = M.insert i 1 table }

incFCost i n = do
  table <- gets float_cost
  b     <- gets bias
  case M.lookup i table of
    Just c  -> modify $ \e -> e{ float_cost = M.insert i (c + n + b) table }
    Nothing -> modify $ \e -> e{ float_cost = M.insert i 1 table }

incVCost v n = do
  modify $ \e -> e{
    var_cost = M.adjust (\(c, u) -> (c + n + bias e, u)) v (var_cost e)
    }

addVar v u = modify $ \e -> e{ var_cost = M.insert v (0, u) (var_cost e) }
incBias n = modify $ \e -> e{ bias = bias e + n }

estimateCost :: CLambda -> ChocoM (Var -> Cost, Int -> Cost, Float -> Cost)
estimateCost ulam = do
  env <- execStateT (walk ulam) initCostEnv
  return $ gen_checker (var_cost env) (int_cost env) (float_cost env)
  where
  walk (Uvar v) = incVCost v 1
  walk (Ulit (IntC n)) | not (isUnsignedImm10 n) = incICost n 1
  walk (Ulit (FloatC f)) = incFCost f 1
  walk (Ulit _) = return ()
  walk (Uapply f args _) = mapM_ walk args
  walk (Ulet (v, e) cont) 
    = do case e of
            Ulit _ | isGlobal v -> addVar v True
            _ -> addVar v False
         walk cont
         n <- getVCost v
         incBias n
         walk e
         incBias (-n)
  walk (Uprim _ args) = mapM_ walk args
  walk (Ucond e1 e2 e3) 
    = do walk e1
         walk e2
         walk e3
  walk (Useq e1 e2)
    = do walk e1
         walk e2
  walk (Uassign v e)
    = do incVCost v 30
         walk e
  walk (Uoffset e _) = walk e
  walk (Uclosure (_, _, body) _)
    = do incBias 2
         walk body
         incBias (-2)

  gen_checker vtab itab ftab =
    let vlst = [ (v, n) | (v, (n, True)) <- M.toList vtab ]
        ilst = M.toList itab
        flst = M.toList ftab
        avg  = fromIntegral (sum (map snd vlst) + 
                sum (map snd ilst) + sum (map snd flst)) /
               fromIntegral (length vlst + length ilst + length flst)
        
        check (Just n) 
          | fromIntegral n > avg * 0.3 = VeryHigh
        check _ = High

        -- fix this routine later
        vcheck v = check( M.lookup v (M.fromList vlst) )
        icheck i = check( M.lookup i itab )
        fcheck f = check( M.lookup f ftab )

    in (vcheck, icheck, fcheck)


{- Closed Lambda to C-- translator Monad -}
type IntTable = M.Map Int StaticLocation
type FloatTable = M.Map Float StaticLocation

type P a = StateT Env ChocoM a
data Env = Env {
  fundecls    :: [(Var, [Var], CLambda)],
  globals     :: M.Map Var StaticLoc,
  int_table   :: IntTable,
  float_table :: FloatTable,
  constants   :: [[DataItem]],
  hsram_next  :: Int,
  greg_next   :: Int,
  var_cost_check   :: Var -> Cost,
  int_cost_check   :: Int -> Cost,
  float_cost_check :: Float -> Cost
  }

initEnv = Env {
  fundecls    = [],
  globals     = M.empty,
  int_table   = M.empty,
  float_table = M.empty,
  constants   = [],
  hsram_next  = hsramBegin,
  greg_next   = globalRegBegin,
  var_cost_check   = undefined,
  int_cost_check   = undefined,
  float_cost_check = undefined
  }

data StaticLoc
  = Memory String
  | HSRAM Int 
  | GlobalReg Int

instance Outputable StaticLoc where
  ppr (Memory label) = text "static memory:" <+> text label
  ppr (HSRAM n)      = text "high speed ram:" <+> int n
  ppr (GlobalReg n)  = text "global register:" <+> int n

addFunction f env = env{ fundecls = f : (fundecls env) }

data Cost 
  = Low
  | High
  | VeryHigh
  deriving (Eq)
  
genStaticLoc :: a -> (a -> Cost) -> P (Maybe StaticLoc)
genStaticLoc v check = do
  greg  <- gets greg_next
  hsram <- gets hsram_next
  case check v of
    VeryHigh 
      | greg < globalRegEnd -> return . Just =<< genGlobalReg v greg
      | hsram < hsramEnd    -> return . Just =<< genHsram v hsram
      | otherwise           -> return Nothing
    High
      | hsram < hsramEnd    -> return . Just =<< genHsram v hsram
      | otherwise           -> return Nothing
    Low -> return Nothing

genGlobalReg v greg = do
  let loc = GlobalReg greg
  modify $ \e -> e{ greg_next = greg + 1 }
  return loc

genHsram v hsram = do
  let loc = HSRAM hsram
  modify $ \e -> e{ hsram_next = hsram + 1 }
  return loc

genMemory v = return (Memory $ toSymbol v)

genVarStaticLoc v = do
  loc <- gets var_cost_check >>= genStaticLoc v
  loc' <- case loc of
    Just l  -> return l
    Nothing -> genMemory v
  lift$putLog (text "assign" <+> ppr v <+> text "to" <+> ppr loc')
  return loc'

findIntConst i = do
  tab <- gets int_table
  case M.lookup i tab of
    Just l  -> return l
    Nothing -> do
      loc <- gets int_cost_check >>= genStaticLoc i
      loc' <- case loc of
        Just (HSRAM n) -> do
          if isUnsignedImm10 i
            then return $ HSRam n Nothing
            else do
              lbl <- lift$newUniq
              return $ HSRam n (Just lbl)
        Just (GlobalReg n) -> do
          if isUnsignedImm10 i
            then return $ SReg n Nothing
            else do
              lbl <- lift$newUniq
              return $ SReg n (Just lbl)
        Nothing -> lift$newUniq >>= return . Static
      modify $ \e -> e{ int_table = M.insert i loc' (int_table e) }
      lift$runIO$print$ text "allocate" <+> int i <+> text "to" <+> ppr loc'
      return loc'

findFloatConst 0.0 = return (SReg 0 Nothing)
findFloatConst f  = do
  tab <- gets float_table
  case M.lookup f tab of
    Just l  -> return l
    Nothing -> do
      loc <- gets float_cost_check >>= genStaticLoc f
      loc' <- case loc of
        Just (HSRAM n) -> do
          lbl <- lift$newUniq
          return $ HSRam n (Just lbl)
        Just (GlobalReg n) -> do
          lbl <- lift$newUniq
          return $ SReg n (Just lbl)
        Nothing -> lift$newUniq >>= return . Static
      modify $ \e -> e{ float_table = M.insert f loc' (float_table e) }
      lift$runIO$print$ text "allocate" <+> float f <+> text "to" <+> ppr loc'
      return loc'

{- Static allocated variables -}
emitConstant :: String -> Expr -> P ()
emitConstant label cst 
  = let d = case cst of
              Cconst_int n  ->  [Cglobal_symbol label, Cint n]
              Cconst_float f -> [Cglobal_symbol label, Cfloat f]
              _ -> panic "emitConstant"
    in modify $ \e -> e{ constants = d : constants e }

setStaticVariable :: StaticLoc -> CLambda -> P (Maybe Expr)
setStaticVariable (Memory label) e
  = do e' <- transl e
       case e' of
        Cconst_int n  -> do
          modify $ \e -> e{ 
            constants = [Cglobal_symbol label, Cint n] : constants e
            }
          return Nothing
        Cconst_float f -> do
          modify $ \e -> e{
            constants = [Cglobal_symbol label, Cfloat f] : constants e
            }
          return Nothing
        _ -> do
          modify $ \e -> e{
            constants = [Cglobal_symbol label, Cskip 1] : constants e
            }
          return . Just $ Cop Cstore [Cconst_symbol label, e']
setStaticVariable (HSRAM n) e
  = do e' <- transl e
       return . Just $ Cop (Chsw n) [e']
setStaticVariable (GlobalReg n) e
  = do e' <- transl e
       return . Just $ Cop (Cregw n) [e']

setStaticArray label n elem
  = do ret <- toStaticData elem
       case ret of
        Left e -> do
          let d = Cglobal_symbol label : concat (replicate n e)
          modify $ \e -> e{
            constants = d : constants e
            }
          return Nothing
        Right e -> do
          let d = [Cglobal_symbol label, Cskip n]
          modify $ \e -> e{
            constants = d : constants e
            }
          return Nothing
        {-
        Right e | isSimpleCmm e -> do
          let d = [Cglobal_symbol label, Cskip n]
          modify $ \e -> e{
            constants = d : constants e
            }
          return . Just . genSequence $ map
            (\i -> Cop Cstore [addConstInt (Cconst_symbol label) i, e]) 
              [0..n-1]
        Right e -> do
          let d = [Cglobal_symbol label, Cskip n]
          modify $ \e -> e{
            constants = d : constants e
            }
          v <- lift$mkIdent "elem"
          return . Just . Clet v e . genSequence $ map
            (\i -> Cop Cstore [addConstInt (Cconst_symbol label) i, Cvar v]) [0..n-1]
        -}

setStaticTuple label tup
  = do ret <- toStaticData tup
       case ret of
        Left es -> do
          let d = [Cglobal_symbol label] ++ es
          modify $ \e -> e{
            constants = d : constants e
            }
          return Nothing
        Right (Cop Calloc es) -> do
          let d = [Cglobal_symbol label, Cskip (length es)]
          modify $ \e -> e{
            constants = d : constants e
            }
          return Nothing
          {-
          return . Just =<< foldM (\code (i, c) -> do
            if isSimpleCmm c
              then return $ Cseq code $
                Cop Cstore [addConstInt (Cconst_symbol label) i, c]
              else do
                v <- lift$mkIdent "elem"
                return $ Clet v c $
                  Cop Cstore [addConstInt (Cconst_symbol label) i, Cvar v]
            ) (Ctuple []) (zip [0..] es)
          -}


toStaticData ulam = case ulam of
  (Ulit (IntC n))     -> return $ Left [Cint n]
  (Ulit (FloatC f))   -> return $ Left [Cfloat f]
  (Ulit (PointerC 0)) -> return $ Left [Cint 0]
  (Ulit (BoolC b))    -> return $ Left [Cint (boolRepr b)]
  (Uprim PcreateTuple es) 
    -> do es' <- mapM transl es
          return . Right $ Cop Calloc es'
  _ -> do e <- transl ulam
          return (Right e)

{- Translations -}
translConst :: Const -> P Expr
translConst (IntC n)      = return $ Cconst_int n
translConst (FloatC f)    = return $ Cconst_float f
translConst (BoolC b)     = return $ Cconst_int (boolRepr b)
translConst UnitC         = return $ Ctuple []
translConst (PointerC n)  = return $ Cconst_int n

translComparison LamSyn.Ceq = CmmSyn.Ceq
translComparison LamSyn.Cne = CmmSyn.Cne
translComparison LamSyn.Clt = CmmSyn.Clt
translComparison LamSyn.Cgt = CmmSyn.Cgt
translComparison LamSyn.Cle = CmmSyn.Cle
translComparison LamSyn.Cge = CmmSyn.Cge

transl :: CLambda -> P Expr
transl lam = case lam of
  Uvar id 
    | isGlobal id -> do
      global_table <- gets globals
      case M.lookup id global_table of
        Just (Memory label) -> return $ Cconst_symbol label
        Just (HSRAM n)      -> return $ Cop (Chsr n) []
        Just (GlobalReg n)  -> return $ Cop (Cregr n) []
        Nothing -> panic $ "transl:Uvar " ++ show id
    | otherwise -> return $ Cvar (toSymbol id)
  Ulit c  -> do 
    c' <- translConst c
    case c' of
      Cconst_int n | not (isUnsignedImm10 n) -> do 
        loc <- findIntConst n
        case loc of
          Static id -> return $ Cop Cload [Cconst_symbol $ "L."++show id]
          HSRam n _ -> return $ Cop (Chsr n) []
          SReg n _  -> return $ Cop (Cregr n) []
      Cconst_float f -> do
        loc <- findFloatConst f
        case loc of
          Static id -> return $ Cop Cload [Cconst_symbol $ "L."++show id]
          HSRam n _ -> return $ Cop (Chsr n) []
          SReg n _  -> return $ Cop (Cregr n) []
      _ -> return c'

  Uclosure fdecl _ -> do -- fixme!
    modify (addFunction fdecl)
    return (Cconst_int 1)
  Uoffset arg off -> do
    arg' <- transl arg
    if off == 0
      then return arg'
      else return $ Cop Caddi [arg', Cconst_int (off * sizeAddr)]

  Uapply f args p -> do
    args' <- mapM transl args
    return $ Cop (Capply p) (Cconst_symbol (toSymbol f) : args')
  Ulet (v, e) body
    | isGlobal v  -> do
      loc <- genVarStaticLoc v
      modify $ \e -> e{ globals = M.insert v loc (globals e) }
      init <- setStaticVariable loc e
      body' <- transl body
      case init of
        Just code -> return $ Cseq code body'
        Nothing   -> return body'
  Ulet (v, Uprim PcreateArray [arg1, arg2]) body
    | isGlobal v -> do
      loc <- genMemory v
      modify $ \e -> e{ globals = M.insert v loc (globals e) }
      arg1' <- transl arg1
      case arg1' of
        Cconst_int n -> 
          do body' <- transl body
             init <- setStaticArray (toSymbol v) n arg2
             case init of
              Just code -> return $ Cseq code body'
              Nothing   -> return body'
        _ -> panic "transl:Ulet"
    | otherwise -> do 
      arg1' <- transl arg1
      arg2' <- transl arg2
      body' <- transl body
      if isSimpleCmm arg2'
        then do 
          code <- createArray arg1' arg2' 
          return $ Clet (toSymbol v) code body'
        else do
          tmp <- lift $ mkTmpVar "tmp" (toScheme UnknownT)
          code <- createArray arg1' (Cvar (toSymbol tmp))
          return $ Clet (toSymbol v) (Clet (toSymbol tmp) arg2' code) body'
  Ulet (v, e@(Uprim PcreateTuple elems)) body
    | isGlobal v -> do
      loc <- genMemory v
      modify $ \e -> e{ globals = M.insert v loc (globals e) }
      body' <- transl body
      init <- setStaticTuple (toSymbol v) e
      case init of
        Just code -> return $ Cseq code body'
        Nothing   -> return $ body'
  Ulet (v, e@(Uclosure _ _)) body -> do
    transl e
    transl body
  Ulet (v, e) body -> do
    e' <- transl e
    body' <- transl body
    return $ Clet (toSymbol v) e' body'
  Uprim prim as -> do
    as' <- mapM transl as
    case (prim, as') of
      (Pextcall s _ r, args) -> return $ Cop (Cextcall s r) args
      (PcreateArray, [arg1, arg2]) 
        -> do createArray arg1 arg2 
      (PcreateTuple, es)  -> return $ Cop Calloc es
      (Pfill s, [arg1, arg2]) -> genFill s arg1 arg2
      (ParraySet, [arg1, arg2, arg3]) -> arraySet arg1 arg2 arg3
      (p, [arg]) -> return $ translPrim1 p arg
      (p, [arg1, arg2]) -> return $ translPrim2 p arg1 arg2

  Ucond (Uprim Pnot [arg]) ifso ifnot -> do
    transl (Ucond arg ifnot ifso)
  Ucond cond ifso ifnot -> do
    cond' <- transl cond
    ifso' <- transl ifso
    ifnot' <- transl ifnot
    return $ Ccond cond' ifso' ifnot'
  Useq e1 e2 -> do
    e1' <- transl e1
    e2' <- transl e2
    return $ Cseq e1' e2'
  Uassign id e -> do
      e' <- transl e
      global_table <- gets globals
      case M.lookup id global_table of
        Just (Memory label) -> return $ Cop Cstore [Cconst_symbol label, e']
        Just (HSRAM n)      -> return $ Cop (Chsw n) [e']
        Just (GlobalReg n)  -> return $ Cop (Cregw n) [e']
        Nothing -> return $ Cassign (toSymbol id) e'

translPrim1 p arg = case p of
  -- Integer operations
  Pnot  -> negInt arg
  Pnegi -> negInt arg
  -- Floating-point operations
  Pitof -> Cop Citof [arg]
  Pftoi -> Cop Cftoi [arg]
  Pabsf -> Cop Cabsf [arg]
  Pnegf -> negFloat arg
  Psqrt -> Cop Csqrt [arg]
  Pinv  -> Cop Cinv [arg]
  -- I/O
  Pput  -> Cop Cput [arg]
  -- Tuple operation
  PtupleRef n -> getField arg n
  -- Memory operations
  Pfield n -> getField arg n

translPrim2 p arg1 arg2 = case p of
  -- Integer operations
  Paddi -> addInt arg1 arg2
  Psubi -> subInt arg1 arg2
  Pmuli -> mulInt arg1 arg2
  Pdivi -> divInt arg1 arg2
  Pcompi cmp -> Cop (Ccompi (translComparison cmp)) [arg1, arg2]
  -- Floating-point operations
  Paddf -> addFloat arg1 arg2
  Psubf -> subFloat arg1 arg2
  Pmulf -> mulFloat arg1 arg2
  Pdivf -> divFloat arg1 arg2
  Pcompf cmp -> Cop (Ccompf (translComparison cmp)) [arg1, arg2]
  -- Array operations
  ParrayRef -> arrayRef arg1 arg2
  p -> panic $ "translPrim2 : " ++ show p

{- Memory related operations -}
fieldAddress ptr 0 = ptr
fieldAddress ptr n = Cop Caddi [ptr, Cconst_int n]

getField ptr n = Cop Cload [fieldAddress ptr n]
setField ptr n val = Cop Cstore [fieldAddress ptr n, val]
getOffset ptr n = fieldAddress ptr n

arrayRef ptr n = Cop Cload [addInt ptr n]

arrayOffs s ptr n = addInt ptr (mulInt n (Cconst_int s))

arraySet ptr n e = return $ Cop Cstore [addInt ptr n, e]

createArray num init 
  =  return $ Cop (Cextcall "lib_create_array" True) [num, init]

genStore label i e  
  = Cop Cstore [addInt (Cvar label) (Cconst_int i), e]
genFill s addr e | s <= 4 =
  if isSimpleCmm e
    then return $ genSequence $ map (\i -> Cop Cstore [addConstInt addr i, e] ) [0..s-1]
    else do
      tmp <- lift $ mkTmpVar "tmp" (toScheme UnknownT)
      let lbl = toSymbol tmp
      return $ Clet lbl e $ 
        genSequence $ map (\i -> Cop Cstore [addConstInt addr i, Cvar lbl]) [0..s-1]
genFill s addr e
  = return $ Cop (Cextcall "lib_memset" False) [addr, e, Cconst_int s]

genSequence [c] = c
genSequence (c:rem) = Cseq c (genSequence rem)

{- Integers -}
negInt (Cconst_int n) = Cconst_int (-n)
negInt (Cop Csubi [c1, c2]) = Cop Csubi [c2, c1]
negInt c = Cop Csubi [Cconst_int 0, c]

addConstInt c n
  | n == 0    = c
  | n > 0     = Cop Caddi [c, Cconst_int n]
  | otherwise = Cop Csubi [c, Cconst_int (-n)]

addInt c1 c2 = case (c1, c2) of
  (Cop Caddi [c1, Cconst_int n1], Cop Caddi [c2, Cconst_int n2])
    -> addConstInt (Cop Caddi [c1, c2]) (n1 + n2)
  (Cop Caddi [c1, Cconst_int n1], c2)
    -> addConstInt (Cop Caddi [c1, c2]) n1
  (c1, Cop Caddi [c2, Cconst_int n2])
    -> addConstInt (Cop Caddi [c1, c2]) n2
  (Cconst_int n, c) -> addConstInt c n
  (c, Cconst_int n) -> addConstInt c n
  (_, _) -> Cop Caddi [c1, c2]

subInt c1 c2 = case (c1, c2) of
  (Cop Caddi [c1, Cconst_int n1], Cop Caddi [c2, Cconst_int n2])
    -> addConstInt (Cop Csubi [c1, c2]) (n1 - n2)
  (Cop Caddi [c1, Cconst_int n1], c2)
    -> addConstInt (Cop Csubi [c1, c2]) n1
  (c1, Cop Caddi [c2, Cconst_int n2])
    -> addConstInt (Cop Csubi [c1, c2]) (-n2)
  (c1, Cconst_int n) 
    -> addConstInt c1 (-n)
  (_, _) -> Cop Csubi [c1, c2]

mulInt c1 c2 = case (c1, c2) of
  (Cconst_int 0, _) -> c1
  (Cconst_int 1, _) -> c2
  (_, Cconst_int 0) -> c2
  (_, Cconst_int 1) -> c1
  -- (Cconst_int n, _) | n == align n
  --   -> let shift = getShift n
  --       in (iterate (\e -> Cop Clsl [e]) c2) !! shift
  -- (_, Cconst_int n) | n == align n
  --   -> let shift = getShift n
  --       in (iterate (\e -> Cop Clsl [e]) c1) !! shift
  (_, _) -> Cop Cmuli [c1, c2]

divInt c1 c2 = case (c1, c2) of
  (_, Cconst_int 1)    -> c1
  (_, Cconst_int (-1)) -> negInt c1
  (_, Cconst_int n) | n == align n
    -> let shift = getShift n
        in (iterate (\e -> Cop Casr [e]) c1) !! shift
  (_, _) -> Cop Cdivi [c1, c2]

{- Floating-points -}
negFloat (Cconst_float n)     = Cconst_float (-n)
negFloat (Cop Csubf [c1, c2]) = Cop Csubf [c2, c1]
negFloat c                    = Cop Cnegf [c]

addConstFloat c n
  | n == 0    = c
  | n > 0     = Cop Caddf [c, Cconst_float n]
  | otherwise = Cop Csubf [c, Cconst_float (-n)]

addFloat c1 c2 = case (c1, c2) of
  (Cop Caddf [c1, Cconst_float n1], Cop Caddf [c2, Cconst_float n2])
    -> addConstFloat (Cop Caddf [c1, c2]) (n1 + n2)
  (Cop Caddf [c1, Cconst_float n1], c2)
    -> addConstFloat (Cop Caddf [c1, c2]) n1
  (c1, Cop Caddf [c2, Cconst_float n2])
    -> addConstFloat (Cop Caddf [c1, c2]) n2
  (Cconst_float _, _) -> Cop Caddf [c2, c1]
  (_, _) -> Cop Caddf [c1, c2]

subFloat c1 c2 = case (c1, c2) of
  (Cop Caddf [c1, Cconst_float n1], Cop Caddf [c2, Cconst_float n2])
    -> addConstFloat (Cop Csubf [c1, c2]) (n1 - n2)
  (Cop Caddf [c1, Cconst_float n1], c2)
    -> addConstFloat (Cop Csubf [c1, c2]) n1
  (c1, Cop Caddf [c2, Cconst_float n2])
    -> addConstFloat (Cop Csubf [c1, c2]) (-n2)
  (c1, Cconst_float n) 
    -> addConstFloat c1 (-n)
  (_, _) -> Cop Csubf [c1, c2]

mulFloat c1 c2 = case (c1, c2) of
  (Cconst_float 0, _) -> c1
  (Cconst_float 1, _) -> c2
  (_, Cconst_float 0) -> c2
  (_, Cconst_float 1) -> c1
  (_, _) -> Cop Cmulf [c1, c2]

divFloat c1 c2 = case (c1, c2) of
  (_, Cconst_float 1)    -> c1
  (_, Cconst_float (-1)) -> negFloat c1
  (_, Cconst_float n)    -> mulFloat c1 (Cconst_float (1/n))
  (Cconst_float 1, _)    -> Cop Cinv [c2]
  (_, _)                 -> mulFloat c1 (Cop Cinv [c2])

{- Translate all function definitions -}
translAllFunctions already_translated cont = do
  decls <- gets fundecls
  case decls of
    []  -> return cont
    (label, params, body) : rem
      -> do
        modify $ \e -> e{ fundecls = rem }
        if S.member label already_translated
          then translAllFunctions already_translated cont
          else do
            fun <- translFunction label params body
            translAllFunctions (S.insert label already_translated)
              (fun : cont)

translFunction label params body = do
  body' <- transl body
  return $ Cfunction FunDec{
          funName = toSymbol label,
          funArgs = map (\id -> (toSymbol id, getMachType id)) params,
          funBody = body'
          }

getMachType :: Var -> MachType
getMachType v = 
  let TyScheme _ ty = var_type v 
  in case ty of
    FloatT  -> Float
    _       -> Int

{- Interfaces -}
compunit :: CLambda -> P [Cmm]
compunit ulam = do
  glob <- lift$newSymbol "global"
  entry_name <- lift$newSymbol "entry_point"

  initcode <- transl ulam

  let entry_fun = Cfunction FunDec{
    funName = entry_name,
    funArgs = [],
    funBody = initcode
    }

  functions <- translAllFunctions S.empty []
  consts <- gets constants
  return $ (entry_fun : functions) ++ map Cdata (reverse consts)

translCmm :: CLambda -> ChocoM ([Cmm], Int, Int, IntTable, FloatTable)
translCmm ulam = do 
  (vc, ic, fc) <- estimateCost ulam
  (code, env) <- runStateT (compunit ulam) 
      initEnv{ 
        var_cost_check   = vc,
        int_cost_check   = ic,
        float_cost_check = fc
      }
  return (code, hsram_next env, greg_next env, int_table env, float_table env)
