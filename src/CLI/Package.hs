module CLI.Package (run) where

import System.Directory (findExecutable)
import System.Exit (exitFailure)
import System.IO

import Dependency.Install


run :: Int -> [String] -> IO ()
run numProcessors args =
  error "TODO"
{-  do
      manager <- Arguments.parse
      env <- Manager.defaultEnvironment
      result <- Manager.run env manager
      case result of
        Right () ->
            return ()

        Left err ->
            errExit ("\nError: " ++ err ++ newline)
          where
            newline = if last err == '\n' then "" else "\n"
-}