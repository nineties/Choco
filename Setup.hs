module Main (main) where

import Distribution.Simple
import System.Exit
import System.Cmd
import System.Directory
import Control.Exception
import Control.Monad

main = defaultMain
{-
main = defaultMainWithHooks $
            defaultUserHooks {
                runTests = runTestScript
            }

withCurrentDirectory path f = do
    cur <- getCurrentDirectory
    setCurrentDirectory path
    finally f (setCurrentDirectory cur)

runTestScript _ _ _ _ = do
    system "runghc Setup configure --prefix=test"
    code <- system "runghc Setup build"

    case code of
         ExitFailure _  -> do
             putStrLn "ERROR : Compilation failed"
             exitWith (ExitFailure 1)
         _ -> do return ()

    system "runghc Setup copy"
    withCurrentDirectory "test" (system "bin/runtest")
    return ()
-}
