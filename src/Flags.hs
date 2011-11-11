------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Flags ( 
    CFlags(..),
    defaultCFlags
    ) where

data CFlags = CFlags {
  in_name        :: String, -- name of the source file
  out_name       :: String, -- name of the output file (without suffix)
  dump_parser    :: Bool,   -- dump output of the parser
  dump_typecheck :: Bool,   -- dump output of the typechecker
  dump_lambda    :: Bool,   -- dump output of the lambda transformer
  dump_simpl     :: Bool    -- dump simplified lambda
  }

defaultCFlags = CFlags {
  in_name        = "",
  out_name       = "a",
  dump_parser    = False,
  dump_typecheck = False,
  dump_lambda    = False,
  dump_simpl     = False
  }
