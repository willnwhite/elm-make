{-# OPTIONS_GHC -Wall #-}
module Fetch.Artifacts.Paths
    ( stuffDirectory
    , description
    , documentation
    )
    where

import System.FilePath ((</>), (<.>))


stuffDirectory :: FilePath
stuffDirectory =
    "elm-stuff"


description :: FilePath
description =
    "elm-package.json"


documentation :: FilePath
documentation =
    stuffDirectory </> "documentation.json"


