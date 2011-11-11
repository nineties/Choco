------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Outputable ( 
  Outputable(..),
  module Text.PrettyPrint.HughesPJ
  ) where

import Control.Monad.Error
import Text.PrettyPrint.HughesPJ

instance Error Doc where
  noMsg  = empty
  strMsg = text

class Outputable a where
  ppr :: a -> Doc

instance Outputable Doc where
  ppr = id

instance Outputable Bool where
  ppr True = text "True"
  ppr False = text "False"

instance Outputable Int where
  ppr n = int n

instance Outputable () where
  ppr _ = text "()"

instance (Outputable a) => Outputable [a] where
  ppr xs = brackets (fsep (punctuate comma (map ppr xs)))

instance (Outputable a) => Outputable (Maybe a) where
  ppr Nothing = text "Nothing"
  ppr (Just x) = text "Just" <+> ppr x

instance (Outputable a, Outputable b) => Outputable (a, b) where
  ppr (x, y) = parens (sep [ppr x <> comma, ppr y])

instance (Outputable a, Outputable b, Outputable c) 
  => Outputable (a, b, c) where
  ppr (x, y, z) = parens (sep [ppr x <> comma, ppr y <> comma, ppr z])
