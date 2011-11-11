------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module LamSyn (
  Lambda(..), Prim(..), LetKind(..),
  Comparison(..),
  LamMap,
  isPureLambda, isSimpleArgument, hasSideEffect,
  freeVars, occursVar, lambdaSmaller
  ) where

import Const
import Outputable
import Types
import Var

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S

data Lambda
  = Lvar Var
  | Llit Const
  | Lapp Lambda [Lambda] Bool
  | Lfun [Var] Lambda
  | Llet LetKind (Var, Lambda) Lambda
  | Lletrec (Var, Lambda) Lambda
  | Lprim Prim [Lambda]
  | Lcond Lambda Lambda Lambda
  | Lseq Lambda Lambda
  | Lassign Var Lambda
  | Lfor Var Lambda Lambda Lambda
  | Lwhile Lambda Lambda
  deriving (Eq, Show)

data LetKind = Strict | Variable
  deriving (Eq, Show)

data Prim
  = Pextcall String Bool Bool
  | Pnot
  | Pnegi | Paddi | Psubi | Pmuli | Pdivi | Plsl | Pasr
  | Pcompi Comparison
  | Pitof | Pftoi
  | Pabsf | Pnegf | Paddf | Psubf | Pmulf | Pdivf | Psqrt | Pinv
  | Pput | Pget
  | Pcompf Comparison
  | PcreateArray 
  | ParrayRef 
  | ParraySet
  | PcreateTuple
  | PtupleRef Int
  | PtupleSet Int
  | Pfield Int
  | Pfill Int
  | Palloc
  | Pstore
  deriving (Eq, Show)

data Comparison
  = Ceq | Cne | Clt | Cgt | Cle | Cge
  deriving (Eq, Show)

type LamMap = M.Map String ([Lambda] -> Lambda)

isPureLambda :: Lambda -> Bool
isPureLambda (Lvar _) = True
isPureLambda (Llit _) = True
isPureLambda (Lprim p args)
  = case p of
    Pextcall _ _ _ -> False
    PcreateArray -> False
    ParraySet -> False
    PcreateTuple -> False
    PtupleSet _ -> False
    Pput -> False
    Pget -> False
    Pfill _ -> False
    _ -> all isPureLambda args
isPureLambda _ = False

hasSideEffect :: Lambda -> Bool
hasSideEffect lam = case lam of
  Lvar _   -> False
  Llit _   -> False
  Lapp _ _ _ -> True
  Lfun _ _ -> False
  Llet _ (_, e) cont -> hasSideEffect e || hasSideEffect cont
  Lletrec (_, e) cont -> hasSideEffect cont
  Lprim p args -> any hasSideEffect args || case p of
    Pextcall _ haseffect _ -> haseffect
    Pput                 -> True
    Pget                 -> True
    ParraySet        -> True
    PtupleSet _      -> True
    Pfill _              -> True
    _ -> False
  Lcond e1 e2 e3 -> any hasSideEffect [e1, e2, e3]
  Lseq e1 e2     -> any hasSideEffect [e1, e2]
  Lassign v e    -> True
  Lfor _ _ _ _  -> True
  Lwhile e1 e2  -> any hasSideEffect [e1, e2]

isSimpleArgument :: Lambda -> Bool
isSimpleArgument lam
  = case lam of
    Lvar _ -> True
    Llit _ -> True
    _ -> False

freeVars lam
  = execState (iter lam) (S.empty)
  where
  iter (Lvar v) 
    = modify (S.insert v)
  iter (Llit _) = return ()
  iter (Lapp f args _) = do{ iter f; mapM_ iter args }
  iter (Lfun params body) 
    = do iter body 
         modify (\s -> foldr S.delete s params)
  iter (Llet _ (v, e) cont)
    = do iter e
         iter cont
         modify (S.delete v)
  iter (Lletrec (v, e) cont)
    = do iter e
         iter cont
         modify (S.delete v)
  iter (Lprim _ args)
    = mapM_ iter args
  iter (Lcond e1 e2 e3)
    = do{ iter e1; iter e2; iter e3 }
  iter (Lseq e1 e2)
    = do{ iter e1; iter e2 }
  iter (Lassign v e)
    = do modify (S.insert v)
         iter e
  iter (Lfor v e1 e2 e3)
    = do iter e1
         iter e2
         iter e3
         modify (S.delete v)
  iter (Lwhile e1 e2)
    = do iter e1
         iter e2

occursVar var lam 
  = case lam of
  Lvar v -> v == var && not (isGlobal v)
  Llit _ -> False
  Lapp f args _ -> occursVar var f || any (occursVar var) args
  Lfun params body -> occursVar var body
  Llet _ (id, e) body -> occursVar var e || occursVar var body
  Lletrec (id, e) body -> occursVar var e || occursVar var body
  Lprim p args -> any (occursVar var) args
  Lcond e1 e2 e3 -> occursVar var e1 || occursVar var e2 || occursVar var e3
  Lseq e1 e2 -> occursVar var e1 || occursVar var e2
  Lassign v e -> (v == var && not (isGlobal v)) || occursVar var e
  Lfor _ e1 e2 e3 -> any (occursVar var) [e1, e2, e3]

lambdaSmaller :: Lambda -> Int -> Bool
lambdaSmaller lam threshold 
  = case execState (iter lam) (Just 0) of
      Just n -> n <= threshold
      Nothing -> False
  where
  iter (Lvar _) = return ()
  iter (Llit (FloatC _)) = incr 1
  iter (Llit _) = return ()
  iter (Lapp (Lvar _) args _) = incr 2 >> mapM_ iter args
  iter (Lapp f args _) = incr 4 >> iter f >> mapM_ iter args
  iter (Lfun args body) = incr (length args) >> iter body
  iter (Llet _ (id, e) body) = iter e >> iter body
  iter (Lletrec (id, e) body) = iter e >> iter body
  iter (Lprim p args) = incr (primSize p args) >> mapM_ iter args
  iter (Lcond e1 e2 e3) = incr 2 >> iter e1 >> iter e2 >> iter e3
  iter (Lseq e1 e2) = iter e1 >> iter e2
  iter (Lwhile e1 e2) = iter e1 >> iter e2
  iter (Lfor v beg end body) = mapM_ iter [beg, end, body]
  iter (Lassign v e) = incr 1 >> iter e
  incr n = modify (fmap (+ n))

primSize prim args
  = case prim of
    Pextcall _ _ _ -> 2 + length args
    Pabsf -> 2
    Pnegf -> 2; Paddf -> 2; Psubf -> 2; Pmulf -> 2 
    Pdivf -> 4
    Psqrt -> 2
    Pitof -> 2
    Pftoi -> 2
    PcreateArray -> 4
    ParrayRef -> 2
    ParraySet -> 2
    PcreateTuple -> 2 + length args
    PtupleRef _ -> 2
    PtupleSet _ -> 2
    Pfield _ -> 1
    Pfill n -> n
    _ -> 1

instance Outputable Lambda where
  ppr (Lvar v)  = ppr v
  ppr (Llit c)  = ppr c
  ppr (Lapp f args _)
    = parens $ text "apply" <+> ppr f <+> hsep (map ppr args)
  ppr (Lfun args body)
    = parens $ hang (text "fun" <+> (parens.hsep) (map ppr args)) 2 
        (ppr body)
  ppr (Llet str (v, e) body)
    = parens $ hang (text "let") 2 
      (parens $ hang (pprLetKind str <+> ppr v) 2 (ppr e) $$ pprLet body)

  ppr (Lletrec (v, e) body)
    = parens $ hang (text "let") 2
      (parens $ hang (char 'r' <+> ppr v) 2 ( ppr e) $$ pprLet body)

  ppr (Lprim p args)
    = parens $ ppr p <+> hsep (map ppr args)

  ppr (Lcond e1 e2 e3)
    = parens $ text "if" <+> ppr e1 $$ ppr e2 $$ ppr e3

  ppr (Lseq e1 e2)
    = parens $ hang (text "seq") 2 (sep [ppr e1, pprSeq e2])
    where
    pprSeq (Lseq e1 e2) = pprSeq e1 $$ pprSeq e2
    pprSeq e = ppr e

  ppr (Lassign v e)
    = parens $ text "assign" <+> ppr v <+> ppr e

  ppr (Lfor i beg end body)
    = parens $ hang (text "for" <+> ppr i <+> text "in" <+> ppr beg <+> text ".." <+> ppr end) 2 (ppr body)

  ppr (Lwhile e1 e2)
    = parens $ hang (text "while" <+> ppr e1) 2 (ppr e2)

pprLet (Llet str (v', e') body') 
  = hang (pprLetKind str <+> ppr v') 2 (ppr e') $$ pprLet body'
pprLet (Lletrec (v', e') body') 
  = hang (char 'r' <+> ppr v') 2 (ppr e') $$ pprLet body'
pprLet e = ppr e

pprLetKind Strict   = char 's'
pprLetKind Variable = char 'v'

instance Outputable Prim where
  ppr (Pextcall name _ _) = text "extcall" <+> text name
  ppr Pnot  = text "not"
  ppr Pnegi = char '~'
  ppr Paddi = char '+'
  ppr Psubi = char '-'
  ppr Pmuli = char '*'
  ppr Pdivi = char '/'
  ppr Plsl  = text "lsl"
  ppr Pasr  = text "asr"
  ppr (Pcompi Ceq) = text "=="
  ppr (Pcompi Cne) = text "!="
  ppr (Pcompi Clt) = char '<'
  ppr (Pcompi Cgt) = char '>'
  ppr (Pcompi Cle) = text "<="
  ppr (Pcompi Cge) = text ">="
  ppr Pitof = text "float_of_int"
  ppr Pftoi = text "int_of_float"
  ppr Pabsf = text "fabs"
  ppr Pnegf = text "~."
  ppr Paddf = text "+."
  ppr Psubf = text "-."
  ppr Pmulf = text "*."
  ppr Pdivf = text "/."
  ppr Psqrt = text "sqrt"
  ppr Pput  = text "put"
  ppr Pget  = text "get"
  ppr (Pcompf Ceq) = text "==."
  ppr (Pcompf Cne) = text "!=."
  ppr (Pcompf Clt) = text "<."
  ppr (Pcompf Cgt) = text ">."
  ppr (Pcompf Cle) = text "<=."
  ppr (Pcompf Cge) = text ">=."
  ppr PcreateArray = text "create_array"
  ppr ParrayRef    = text "array_ref"
  ppr ParraySet    = text "array_set"
  ppr PcreateTuple = text "tuple"
  ppr (PtupleRef i) = text "get#" <> int i 
  ppr (PtupleSet i) = text "set#" <> int i
  ppr (Pfield n) = text "field" <+> int n
  ppr (Pfill n)  = text "fill" <+> int n
  ppr Palloc = text "alloc"
  ppr Pstore = text "store"
