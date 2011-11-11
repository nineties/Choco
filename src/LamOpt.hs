------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module LamOpt (
  optimizeLambda
  ) where

import Choco
import Id
import LamSyn
import Outputable
import SrcLoc
import Var

import ArrayOpt
import Contract
import ElimLet

{- Interface -}
commandTable = [
  ('l', simplifyLets),
  ('a', arrayOpt),
  ('i', doContract)
  ]

optimizeLambda :: String -> Lambda -> ChocoM Lambda
optimizeLambda [] lam = return lam
optimizeLambda (c:rem) lam = case lookup c commandTable of
  Just m -> do lam' <- m lam
               optimizeLambda rem lam'
  Nothing -> simpleError (text "unknown optimization command: " <> char c)
