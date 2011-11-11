------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module CLamSyn (
  CLambda(..), occursVar
  ) where

import Const
import LamSyn hiding (occursVar)
import Outputable
import Types
import Var

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S

data CLambda
  = Uvar Var
  | Ulit Const
  | Uapply Var [CLambda] Bool -- true if it returns non unit value
  | Ulet (Var, CLambda) CLambda
  | Uprim Prim [CLambda]
  | Ucond CLambda CLambda CLambda
  | Useq CLambda CLambda
  | Uwhile CLambda CLambda
  | Ufor Var CLambda CLambda CLambda
  | Uassign Var CLambda
  | Uoffset CLambda Int
  | Uclosure (Var, [Var], CLambda) [CLambda]
  deriving (Eq, Show)

instance Outputable CLambda where
  ppr (Uvar v)  = ppr v
  ppr (Ulit c)  = ppr c
  ppr (Uapply f args _)
    = parens $ text "apply" <+> ppr f <+> hsep (map ppr args)
  ppr (Ulet (v, e) body)
    = parens $ hang (text "let") 2 
      (parens $ hang (ppr v) 2 (ppr e) $$ pprLet body)
  ppr (Uprim p args)
    = parens $ ppr p <+> hsep (map ppr args)

  ppr (Ucond e1 e2 e3)
    = parens $ text "if" <+> ppr e1 $$ ppr e2 $$ ppr e3

  ppr (Useq e1 e2)
    = parens $ hang (text "seq") 2 (sep [ppr e1, pprSeq e2])
    where
    pprSeq (Useq e1 e2) = pprSeq e1 $$ pprSeq e2
    pprSeq e = ppr e

  ppr (Uwhile e1 e2)
    = parens $ text "while" <+> ppr e1 $$ ppr e2

  ppr (Ufor v e1 e2 e3)
    = parens $ text "for" <+> ppr v <+> text "in" <+> 
        ppr e1 <+> text ".." <+> ppr e2 $$ ppr e3
  ppr (Uassign v e)
    = parens $ text "assign" <+> ppr v <+> ppr e

  ppr (Uoffset e n)
    = parens $ text "offset" <+> ppr e <+> ppr n

  ppr (Uclosure (f, params, body) _)
    = (parens $ hang (text "fun" <+> ppr f <+> (parens.hsep) (map ppr params))
       2 (ppr body))

pprLet (Ulet (v', e') body') 
  = hang (ppr v') 2 (ppr e') $$ pprLet body'
pprLet e = ppr e

occursVar :: Var -> CLambda -> Bool
occursVar var lam = case lam of
  Uvar v             -> v == var && not (isGlobal v)
  Ulit _             -> False
  Uapply f args _ -> any (occursVar var) args
  Ulet (v, e) cont   -> occursVar var e || occursVar var cont
  Uprim p args       -> any (occursVar var) args
  Ucond l1 l2 l3     -> any (occursVar var) [l1, l2, l3]
  Useq l1 l2         -> any (occursVar var) [l1, l2]
  Uwhile l1 l2       -> any (occursVar var) [l1, l2]
  Ufor v l1 l2 l3    -> any (occursVar var) [l1, l2, l3]
  Uassign v e        -> (v == var && not (isGlobal v)) || occursVar var e
  Uoffset e _        -> occursVar var e
  Uclosure _ clos    -> any (occursVar var) clos

