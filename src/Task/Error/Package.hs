{-# OPTIONS_GHC -Wall #-}
module Task.Error.Package where

import qualified Elm.Package as Pkg


-- ERRORS

data Error
    = ZipError Pkg.Name Pkg.Version
    | SolverError


toString :: Error -> String
toString err =
  error "TODO" err
