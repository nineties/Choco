------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Log (
  CLog(..),
  CLogs
  ) where

import Data.Sequence
import Text.PrettyPrint.HughesPJ

data CLog
  = LargeLog {
    title   :: String,
    body    :: Doc
    }
  | SmallLog Doc
  | DebugLog String

type CLogs = Seq CLog
