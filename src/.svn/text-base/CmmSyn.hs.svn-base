------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module CmmSyn (
    MachType(..),
    voidType, intType, floatType,
    size, sizeType,
    Comp(..), negateComp, swapComp,
    MemoryChunk(..), Operation(..), Expr(..), FunDec(..), DataItem(..),
    Cmm(..),
    isSimpleCmm,

    -- pretty printing
    pprMachType
    ) where

import Arch 
import Outputable

type Ident = String -- temporal

data MachType = Int | Float
    deriving (Eq, Show)

voidType  = []
intType   = [Int]
floatType = [Float]

size Int    = Arch.sizeInt
size Float  = Arch.sizeFloat

sizeType = sum . map size


data Comp
    = Ceq | Cne | Clt | Cle | Cgt | Cge
    deriving (Eq, Show)

negateComp Ceq = Cne
negateComp Cne = Ceq
negateComp Clt = Cge
negateComp Cle = Cgt
negateComp Cgt = Cle
negateComp Cge = Clt

swapComp Ceq = Ceq
swapComp Cne = Cne
swapComp Clt = Cgt
swapComp Cgt = Clt
swapComp Cle = Cge
swapComp Cge = Cle

data MemoryChunk 
  = Mem_Integer
  | Mem_Float
  | Mem_Addr
  deriving (Eq)
  
data Operation
    = Capply Bool
    | Cextcall String Bool
    | Calloc
    | Cload 
    | Cstore 
    | Chsw Int | Chsr Int
    | Cregw Int | Cregr Int
    | Cfill Int
    | Cnegi | Caddi | Csubi | Cmuli | Cdivi | Clsl | Casr
    | Ccompi Comp
    | Cabsf | Cnegf | Caddf | Csubf | Cmulf | Cdivf | Csqrt | Cinv
    | Ccompf Comp
    | Cftoi | Citof
    | Cput
    deriving (Show, Eq)

data Expr
    = Cconst_int Int
    | Cconst_float Float
    | Cconst_symbol String
    | Cvar Ident
    | Clet Ident Expr Expr
    | Cassign Ident Expr
    | Ctuple [Expr]
    | Cop Operation [Expr]
    | Cseq Expr Expr
    | Ccond Expr Expr Expr
    deriving (Show)

isSimpleCmm (Cconst_int n)    = True
isSimpleCmm (Cconst_float f)  = True
isSimpleCmm (Cconst_symbol s) = True
isSimpleCmm (Cop (Cregr _) _) = True
isSimpleCmm (Cop (Cregw _) _) = True
isSimpleCmm (Cvar _)          = True
isSimpleCmm _                 = False

data FunDec = FunDec {
    funName :: Ident,
    funArgs :: [(Ident, MachType)],
    funBody :: Expr
    }

data DataItem
    = Cglobal_symbol String
    | Cint Int
    | Cfloat Float
    | Cskip Int
    | Cstatic_array Int Int
    | Cstatic_tuple Int


data Cmm
  = Cdata [DataItem]
  | Cfunction FunDec


instance Outputable MachType where
  ppr Int   = text "int"
  ppr Float = text "float"

pprMachType :: [MachType] -> Doc
pprMachType [] = text "unit"
pprMachType ts = hsep $ punctuate (char '*') (map ppr ts)

instance Outputable Comp where
  ppr Ceq = text "=="
  ppr Cne = text "!="
  ppr Clt = char '<'
  ppr Cgt = char '>'
  ppr Cle = text "<="
  ppr Cge = text ">="

instance Outputable MemoryChunk where
  ppr Mem_Integer = text "int32"
  ppr Mem_Float   = text "float32"
  ppr Mem_Addr    = text "addr32"

instance Outputable Operation where
  ppr (Capply _)   = text "call"
  ppr (Cextcall s _) = text "extcall" <+> text s
  ppr Cload        = text "load"
  ppr Calloc       = text "alloc"
  ppr Cstore       = text "store"
  ppr (Chsw n)     = text "hsw" <+> int n
  ppr (Chsr n)     = text "hsr" <+> int n
  ppr (Cregw n)    = text "regw" <+> int n
  ppr (Cregr n)    = text "regr" <+> int n
  ppr (Cfill n)    = text "fill" <+> int n
  ppr Cnegi        = text "negi"
  ppr Caddi        = text "addi"
  ppr Csubi        = text "subi"
  ppr Cmuli        = text "muli"
  ppr Cdivi        = text "divi"
  ppr Clsl         = text "lsl"
  ppr Casr         = text "asr"
  ppr (Ccompi c)   = ppr c
  ppr Cabsf        = text "fabs"
  ppr Cnegf        = text "fneg"
  ppr Caddf        = text "fadd"
  ppr Csubf        = text "fsub"
  ppr Cmulf        = text "fmul"
  ppr Cdivf        = text "fdiv"
  ppr Csqrt        = text "fsqrt"
  ppr Cinv         = text "finv"
  ppr (Ccompf c)   = ppr c <> char '.'
  ppr Cftoi        = text "ftoi"
  ppr Citof        = text "itof"
  ppr Cput         = text "put"

instance Outputable Expr where
  ppr (Cconst_int n)     = int n
  ppr (Cconst_float f)   = float f
  ppr (Cconst_symbol s)  = text s
  ppr (Cvar s) = text s
  ppr (Clet id e cont)   
    = parens (text "let" <+> text id <+> ppr e $$ ppr cont)
  ppr (Cassign s e) = parens $ text "assign" <+> text s <+> ppr e
  ppr (Ctuple []) = text "()"
  ppr (Ctuple es) = parens $ (text "tuple") <+> (hsep (map ppr es))
  ppr (Cop op args) = parens $ ppr op <+> hsep (map ppr args)
  ppr (Cseq e1 e2) = parens (ppr e1 $$ pprSeq e2)
  ppr (Ccond e1 e2 e3) = 
    parens $ hang (text "if" <+> ppr e1) 2 (ppr e2 $$ ppr e3)

pprSeq (Cseq e1 e2) = ppr e1 $$ pprSeq e2
pprSeq e = ppr e

instance Outputable FunDec where
  ppr f = parens $ hang (text "function" <+> text (funName f) <+>
    parens pprArgs) 2 (parens.ppr.funBody $  f)
    where
    pprArgs = hsep $ 
      map (\(id, ty) -> text id <> char ':' <> ppr ty) (funArgs f)

instance Outputable DataItem where
  ppr (Cglobal_symbol s)  = text "global" <+> doubleQuotes (text s)
  ppr (Cint n)            = text "int32" <+> int n
  ppr (Cfloat f)          = text "float32" <+> float f
  ppr (Cskip n)           = text "skip" <+> int n
  ppr (Cstatic_array s n) = text "array" <+> int s <+> int n
  ppr (Cstatic_tuple n)   = text "tuple" <+> int n

instance Outputable Cmm where
  ppr (Cdata dl) = text "data" <+> hsep (map ppr dl)
  ppr (Cfunction f) = ppr f
