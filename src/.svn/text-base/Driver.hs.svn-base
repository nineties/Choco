------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Driver (
  getCFlags
  ) where

import Choco

import Control.Monad
import Data.List
import System.Console.GetOpt
import System.IO.Unsafe
import System.Environment
import System.Exit

data Opt
  = OutName String
  | DParser
  | DTypeCheck
  | DUnpoly
  | DLambda
  | DSimpl
  | DClose
  | DCmm
  | DSelection
  | DLcse
  | DCombine
  | DLive
  | DSpill
  | DSplit
  | DLinear
  | DScheduling
  | DInterf
  | DRegalloc
  | DReload
  | OptCmd String
  | InlineThreshold String
  | Asm String
  | Base String
  | NopFlag
  | Help

progName :: String
progName = unsafePerformIO (getProgName)

usage :: String
usage = usageInfo ("Usage : " ++ progName ++ " <sourcefile> [options]") options

options :: [OptDescr Opt]
options =
  [ Option ['h'] ["help"]   (NoArg Help)  "show this help"
  , Option ['o'] ["ofile"]  (ReqArg OutName "OUTPUT_FILE")
    $ "output file (default is FILENAME.as)"
  , Option [] ["dump-parser"] (NoArg DParser)
    "dump parser output"
  , Option [] ["dump-typecheck"]  (NoArg DTypeCheck)
    "dump code after typecheck"
  , Option [] ["dump-unpoly"] (NoArg DUnpoly)
    "dump code after specification of polymorphic functions"
  , Option [] ["dump-lambda"] (NoArg DLambda)
    "dump code after lambda transformation"
  , Option [] ["dump-simpl"] (NoArg DSimpl)
    "dump simplified lambda"
  , Option [] ["dump-close"] (NoArg DClose)
    "dump closed lambda"
  , Option [] ["dump-cmm"] (NoArg DCmm)
    "dump code of c--"
  , Option [] ["dump-selection"] (NoArg DSelection)
    "dump code after instruction selection"
  , Option [] ["dump-lcse"] (NoArg DLcse)
    "dump code after local common subexpression elimination"
  , Option [] ["dump-combine"] (NoArg DCombine)
    "dump code after allocation combining"
  , Option [] ["dump-live"] (NoArg DLive)
    "dump result of liveness analysis"
  , Option [] ["dump-spill"] (NoArg DSpill)
    "dump code after spilling"
  , Option [] ["dump-split"] (NoArg DSplit)
    "dump code after live range splitting"
  , Option [] ["dump-linear"] (NoArg DLinear)
    "dump linearized code"
  , Option [] ["dump-scheduling"] (NoArg DScheduling)
    "dump code after instruction scheduling"
  , Option [] ["dump-interf"] (NoArg DInterf)
    "dump interferences"
  , Option [] ["dump-regalloc"] (NoArg DRegalloc)
    "dump code after regiater allocation"
  , Option [] ["dump-reload"] (NoArg DReload)
    "dump code after insertion of reloading code"
  , Option [] ["opt"] (ReqArg OptCmd "[lia]*")
    $ "optimize lambda by specified optimization sequence\n" ++
      "\tl : eliminate unnecessary let bindings" ++
      "\ti : perform inlining" ++
      "\ta : optimize array operations"
  , Option [] ["inline"] (ReqArg InlineThreshold "THRESHOLD")
    "inline thresholod"
  , Option [] ["asm"] (ReqArg Asm "PATH_TO_ASSEMBLER")
    "generate binary by specified assembler"
  , Option [] ["base"] (ReqArg Base "BASE_ADDRESS")
    "base address"
  , Option [] ["withnop"] (NoArg NopFlag)
    "use nop-flag"
  ]

getOpts :: [String] -> IO ([Opt], [String])
getOpts argv =
    case getOpt Permute options argv of
         (o, n,   []) -> return (o, n)
         (_, _, errs) -> ioError (userError (concat errs ++ usage))

buildFlags :: ([Opt], [String]) -> IO CFlags
buildFlags (opts, inputs) = 
    if null inputs 
      then foldM dispatch defaultCFlags opts
      else let i = head inputs
               out = if ".ml" `isSuffixOf` i
                      then fst (splitAt (length i - 3) i) ++ ".as"
                      else i ++ ".as"
            in foldM dispatch 
                  defaultCFlags{ in_name = i,
                                 out_name = out
                                } opts
    where
    dispatch _ Help              = putStrLn usage >> exitWith ExitSuccess
    dispatch info (OutName file) = return info{ out_name = file}
    dispatch info DParser      = return info{ dump_parser    = True }
    dispatch info DTypeCheck   = return info{ dump_typecheck = True }
    dispatch info DUnpoly      = return info{ dump_unpoly    = True }
    dispatch info DLambda      = return info{ dump_lambda    = True }
    dispatch info DSimpl       = return info{ dump_simpl     = True }
    dispatch info DClose       = return info{ dump_close     = True }
    dispatch info DCmm         = return info{ dump_cmm       = True }
    dispatch info DSelection   = return info{ dump_selection = True }
    dispatch info DLcse        = return info{ dump_lcse      = True }
    dispatch info DCombine     = return info{ dump_combine   = True }
    dispatch info DLive        = return info{ dump_live      = True }
    dispatch info DSpill       = return info{ dump_spill     = True }
    dispatch info DSplit       = return info{ dump_split     = True }
    dispatch info DLinear      = return info{ dump_linear    = True }
    dispatch info DScheduling  = return info{ dump_scheduling = True }
    dispatch info DInterf      = return info{ dump_interf    = True }
    dispatch info DRegalloc    = return info{ dump_regalloc  = True }
    dispatch info DReload      = return info{ dump_reload    = True }
    dispatch info (OptCmd cmd) = return info{ opt_command  = cmd }
    dispatch info NopFlag      = return info{ use_nopflag  = True }
    dispatch info (Asm cmd)    = return info{ assembler = cmd }
    dispatch info (Base base)  = return info{ address_base = read base }
    dispatch info (InlineThreshold s)
      = do let t = (read s) :: Int
           if t < 0
              then putStrLn usage >> exitWith ExitSuccess
              else return info{ inline_threshold = t }

getCFlags :: IO CFlags
getCFlags = getArgs >>= getOpts >>= buildFlags
