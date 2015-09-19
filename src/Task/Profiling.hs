{-# OPTIONS_GHC -Wall #-}
module Reporting.Profiling where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (StateT, liftIO, runStateT)
import qualified Control.Monad.State as State
import qualified Data.List as List
import qualified Data.Time.Clock.POSIX as Time
import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg
import qualified Elm.Package.Paths as Path
import System.FilePath ((</>))

import qualified Report
import qualified TheMasterPlan as TMP


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
