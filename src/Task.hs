{-# OPTIONS_GHC -Wall #-}
module Task where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (StateT, liftIO, runStateT)
import qualified Control.Monad.State as State
import qualified Data.Time.Clock.POSIX as Time
import System.Directory (findExecutable)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import qualified Task.Error as Error


-- TASKS
-- handles errors, tracks profiling information, holds configuration info

type Task =
  ExceptT ExitReason (StateT [Phase] IO)


data ExitReason
    = Error Error.Error
    | Quit


throw :: Error.Error -> Task a
throw err =
  throwError (Error err)


quit :: Task a
quit =
  throwError Quit


-- RUN A TASK

run :: Task a -> IO (Either ExitReason (a, Timeline))
run task =
  do  requireGit
      result <-
          runStateT (runExceptT (phase "overall" task)) []
      case result of
        (Right answer, [Phase _ start phases end]) ->
            return (Right (answer, Timeline start phases end))

        (Left err, _) ->
            return (Left err)

        (Right _, _) ->
            error "Something impossible happened when profiling."


requireGit :: IO ()
requireGit =
  do  maybePath <- findExecutable "git"
      case maybePath of
        Just _  ->
            return ()

        Nothing ->
            do  hPutStrLn stderr gitNotInstalledMessage
                exitFailure


gitNotInstalledMessage :: String
gitNotInstalledMessage =
    "\n\
    \Elm relies on git to download libraries and manage versions. It appears that\n\
    \you do not have git installed though!\n\n\
    \Get it from <http://git-scm.com/downloads> to continue."


-- TIMELINE

data Timeline = Timeline
    { _start :: Time.POSIXTime
    , _phases :: [Phase]
    , _end :: Time.POSIXTime
    }


data Phase = Phase
    { _tag :: String
    , _start_ :: Time.POSIXTime
    , _subphases :: [Phase]
    , _end_ :: Time.POSIXTime
    }


phase :: String -> Task a -> Task a
phase name task =
  do  phasesSoFar <- State.get
      State.put []
      start <- liftIO Time.getPOSIXTime
      result <- task
      end <- liftIO Time.getPOSIXTime
      State.modify' (\phases -> Phase name start (reverse phases) end : phasesSoFar)
      return result


timelineToString :: Timeline -> String
timelineToString (Timeline start phases end) =
  let
    duration = end - start
  in
    "\nOverall time: " ++ show duration ++ "\n"
    ++ concatMap (phaseToString duration 1) phases
    ++ "\n"


phaseToString :: Time.POSIXTime -> Int -> Phase -> String
phaseToString overallDuration indent (Phase tag start subphases end) =
  let
    duration = end - start
    percent = truncate (100 * duration / overallDuration) :: Int
  in
    '\n' : replicate (indent * 4) ' ' ++ show percent ++ "% - " ++ tag
    ++ concatMap (phaseToString duration (indent + 1)) subphases
