{-# OPTIONS_GHC -fglasgow-exts #-}
------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Choco (
  ChocoM,
  CFlags(..),
  CEnv(..),
  defaultCFlags,

  -- Choco APIs
  execChoco,
  compileError,
  simpleError,
  getFlag,
  report,
  putLog,
  trace,
  runIO,
  newUniq,
  newSymbol,

  ) where

import Outputable
import SrcLoc
import Reg

import Control.Monad.Error
import Control.Monad.Reader.Class
import Control.Monad.RWS
import qualified Data.Map as M
import Data.Sequence
import Data.Traversable
import System.Exit
import Text.PrettyPrint.HughesPJ

-- Monad for choco
newtype ChocoM a = ChocoM {
  unChoco :: ErrorT CError (RWST CFlags CLogs CEnv IO) a
  }

data CEnv = CEnv {
  uniq_source     :: !Int,
  num_stack_slots :: !Int,  -- fixme!!
  contains_calls  :: Bool,
  reg_list        :: [Reg],
  reg_info_table  :: M.Map Reg RegInfo,
  phys_regs       :: [Reg],
  reg_stamp       :: Int,
  fun_reg_info   :: M.Map String [Reg]
  }
  deriving (Show)

initCEnv = CEnv {
  uniq_source     = normalVarIdStart,
  num_stack_slots = 0,
  contains_calls  = False,
  reg_list        = [],
  reg_info_table  = M.empty,
  phys_regs       = [],
  reg_stamp       = 0,
  fun_reg_info   = M.empty
  }

normalVarIdStart = 1000

{- Flags -}
data CFlags = CFlags {
  in_name          :: String, -- name of the source file
  out_name         :: String, -- name of the output file (without suffix)
  dump_parser      :: Bool,   -- dump output of the parser
  dump_typecheck   :: Bool,   -- dump output of the typechecker
  dump_unpoly      :: Bool,   -- dump output of the type specification
  dump_lambda      :: Bool,   -- dump output of the lambda transformer
  dump_simpl       :: Bool,   -- dump simplified lambda
  dump_close       :: Bool,   -- dump closed lambda
  dump_cmm         :: Bool,   -- dump cmm code
  dump_selection   :: Bool,
  dump_lcse        :: Bool,
  dump_combine     :: Bool,
  dump_live        :: Bool,
  dump_spill       :: Bool,
  dump_split       :: Bool,
  dump_linear      :: Bool,
  dump_scheduling  :: Bool,
  dump_interf      :: Bool,
  dump_regalloc    :: Bool,
  dump_reload      :: Bool,
  opt_command      :: String,
  assembler        :: String,
  inline_threshold :: Int,
  address_base     :: Int,
  use_nopflag      :: Bool
  }

defaultCFlags = CFlags {
  in_name          = "",
  out_name         = "",
  dump_parser      = False,
  dump_typecheck   = False,
  dump_unpoly      = False,
  dump_lambda      = False,
  dump_simpl       = False,
  dump_close       = False,
  dump_cmm         = False,
  dump_selection   = False,
  dump_combine     = False,
  dump_lcse        = False,
  dump_live        = False,
  dump_spill       = False,
  dump_split       = False,
  dump_linear      = False,
  dump_scheduling  = False,
  dump_interf      = False,
  dump_regalloc    = False,
  dump_reload      = False,
  opt_command      = "lialialialialialaialialialialailaialialialia",
  assembler        = "",
  inline_threshold = 100,
  address_base     = 4096,
  use_nopflag      = False
  }

{- Error -}
data CError
  = CompileError Doc SrcLoc
  | InternalError Doc
  deriving (Show)

instance Error CError where
  strMsg msg = InternalError (text msg)

{- Logging -}
data CLog
  = LargeLog {
    title   :: String,
    body    :: Doc
    }
  | SmallLog Doc
  | DebugLog String
  deriving (Show)

type CLogs = Seq CLog

instance Outputable CLog where
  ppr (LargeLog title body) 
    = text "=====" <+> text title <+> text "=====" $$ body

  ppr (SmallLog msg) = msg

  ppr (DebugLog msg) = text "Debug:" <+> text msg
      
{- Choco APIs -}
execChoco (ChocoM m) flags = do
  (result,_,logs) <- runRWST (runErrorT m) flags initCEnv

  {-
  for logs $ \log ->
    putStrLn $ render (ppr log)
  -}

  case result of
    Left err -> print err
    Right _ -> return ()

compileError :: Doc -> SrcLoc -> ChocoM a
compileError msg loc = throwError (CompileError msg loc)

simpleError :: Doc -> ChocoM a
simpleError msg = throwError (CompileError msg noSrcLoc)

runIO :: IO a -> ChocoM a
runIO = liftIO

getFlag :: (CFlags -> a) -> ChocoM a
getFlag = asks

newUniq :: ChocoM Int
newUniq = do env@CEnv{ uniq_source = n } <- get
             put env{ uniq_source = n+1 }
             return n

newSymbol :: String -> ChocoM String
newSymbol str = newUniq >>= return . (str ++) . show

report :: String -> Doc -> ChocoM ()
report title doc = runIO . putStrLn . render $ ppr (LargeLog title doc)

putLog :: Doc -> ChocoM ()
putLog msg = runIO . putStrLn $ render msg

trace :: Show a => a -> ChocoM ()
trace a = tell $ singleton (DebugLog (show a))

{- Instanciation -}
instance Monad ChocoM where
  return a = ChocoM (return a)
  (ChocoM m) >>= f = ChocoM $
    do a <- m
       unChoco (f a)

instance MonadError CError ChocoM where
  throwError e = ChocoM (throwError e)
  catchError (ChocoM m) f = ChocoM (catchError m (unChoco.f))

instance MonadReader CFlags ChocoM where
  ask = ChocoM ask
  local f (ChocoM m) = ChocoM (local f m)

instance MonadWriter CLogs ChocoM where
  tell w = ChocoM (tell w)
  listen (ChocoM m) = ChocoM (listen m)
  pass (ChocoM f) = ChocoM (pass f)

instance MonadState CEnv ChocoM where
  get = ChocoM get
  put a = ChocoM (put a)
  
instance MonadIO ChocoM where
  liftIO m = ChocoM (liftIO m)
