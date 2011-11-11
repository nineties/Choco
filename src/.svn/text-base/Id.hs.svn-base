------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Id ( 
  Id, genIdSource, newId, pprId,
  IdMap, IdSet
  ) where

import Outputable

import Data.IORef
import qualified Data.IntMap as I
import qualified Data.Set as S
import System.IO.Unsafe

{- identifiers -}


-----------------------------------------------------------
--  identification number
-----------------------------------------------------------

type Id      = Int
type IdMap e = I.IntMap e
type IdSet   = S.Set Id

genIdSource :: Int -> IORef Int
genIdSource = unsafePerformIO . newIORef

newId :: IORef Int -> Id
newId source = unsafePerformIO $
    do n <- readIORef source
       writeIORef source (n+1)
       return n

pprId :: Id -> Doc
pprId = int
{-# NOINLINE genIdSource #-}
{-# NOINLINE newId #-}
