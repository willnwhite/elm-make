{-# OPTIONS_GHC -Wall #-}
module Task.Error where

import qualified Task.Error.Make as Make
import qualified Task.Error.Package as Package


data Error
    = Abort
    | Make Make.Error
    | Package Package.Error


toString :: Error -> String
toString err =
  case err of
    Abort ->
        ""

    Make makeErr ->
        Make.toString makeErr

    Package pkgErr ->
        Package.toString pkgErr