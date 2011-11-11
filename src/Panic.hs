{-# OPTIONS_GHC -fglasgow-exts #-}
------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Panic
    ( ChocoException(..)
    , panic
    ) where

import Control.Exception
import Data.Typeable
import System.IO.Unsafe ( unsafePerformIO )
import System.Environment
import System.Exit

{- functions for error handling -}

progName = unsafePerformIO (getProgName)
{-# NOINLINE progName #-}

data ChocoException
    = Panic String
    deriving (Eq, Typeable)

instance Show ChocoException where
    show e = progName ++ ": " ++ showChocoException e

showChocoException (Panic s)
    = "panic! (the 'impossible' happened): " ++ s

panic :: String -> a
panic x = throwDyn (Panic x)
