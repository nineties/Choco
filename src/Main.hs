{-# OPTIONS -fglasgow-exts #-}
------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Main (main) where

import AsmGen
import Choco
import Closure
import CmmGen
import CmmSyn
import Emit
import Driver
import LamGen
import LamOpt
import Libraries
import Mach
import Outputable
import Panic
import Parser
import Typing

import Control.Exception (catchDyn)
import Control.Monad.Reader
import System.Exit
import System.Cmd

main = errorHandler $ do
  flags <- getCFlags
  execChoco chocoMain flags

errorHandler f =
  f
  `catchDyn`
  (\(exception :: ChocoException)
    -> do print exception
          exitWith (ExitFailure 1))
  `catch`
  (\exception 
    -> do print exception
          exitWith (ExitFailure 1))
    

chocoMain = do
  flags <- ask

  src <- runIO $ if (in_name flags) == ""
    then getContents
    else readFile (in_name flags)

  {- parse source file -}
  putLog (text "parsing ...")
  ast <- parseProgram src
  when (dump_parser flags) $
    report "Abstract syntax" (ppr ast)

  {- type check -}
  putLog (text "checking types ...")
  tystmt <- typeCheck ast
  when (dump_typecheck flags) $
    report "Type check" (ppr tystmt)

  {- expand polymorphic functions -}
  -- unpolystmt <- unpolyProgram tystmt
  -- when (dump_unpoly flags) $
  --   report "Specified types of polumorphic functions" (ppr unpolystmt)

  {- translate to Lambda langugage -}
  putLog (text "translating to lambda language ...")
  lam <- translLambda tystmt

  {- append library function definitions -}
  -- lam <- addLib lam
  when (dump_lambda flags) $
    report "Lambda language" (ppr lam)

  {- simplify -}
  putLog (text "optimizing lambda language ...")
  lam' <- optimizeLambda (opt_command flags) lam
  when (dump_simpl flags) $
    report "Optimized Lambda" (ppr lam')

  {- closure conversion -}
  putLog (text "closure conversion ...")
  ulam <- closeLambda lam'
  when (dump_close flags) $
    report "Closed Lambda language" (ppr ulam)
  
  {- translate to C-- language -}
  putLog (text "translating to C-- ...")
  (cmm, hsram, greg, itab, ftab) <- translCmm ulam
  when (dump_cmm flags) $
    report "C-- code" (ppr cmm)

  {- translate to Virtual Machine code -}
  putLog (text "generating assembly code ...")

  let (fundecls, datas) = divide cmm
  code <- Emit.fundecls (hsram, greg, itab, ftab) datas 
              =<< mapM (asmgen flags itab ftab) fundecls

  if out_name flags == ""
    then report "Assembly code" code
    else runIO $ writeFile (out_name flags) $ render code

  when (assembler flags /= "") $ do
    putLog (text "generateing binary code by " 
       <+> text (assembler flags) <+> text "...")
    runIO $ system $ assembler flags ++ " " ++ out_name flags
    return ()
  return ()

  where
  divide [] = ([], [])
  divide (Cfunction f:rem) = let (fs, cs) = divide rem in (f:fs, cs)
  divide (Cdata c:rem)     = let (fs, cs) = divide rem in (fs, c:cs)
