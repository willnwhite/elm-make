{-# OPTIONS_GHC -Wall #-}
module Task.Error where

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg

import qualified Fetch.Artifacts.Paths as Paths


data Error
    = ImportNotFound Module.Name (Maybe Module.Name)
    | ImportFoundTooMany
        { _name :: Module.Name
        , _parent :: Maybe Module.Name
        , _local :: [FilePath]
        , _foreign :: [Pkg.Name]
        }
    | ModuleNameFileNameMismatch
        { _path :: FilePath
        , _expectedName :: Module.Name
        , _actualName :: Module.Name
        }


toString :: Error -> String
toString err =
  case err of
    ImportNotFound name maybeParent ->
        unlines
          [ "Error when searching for modules" ++ toContext maybeParent ++ ":"
          , "    Could not find module '" ++ Module.nameToString name ++ "'"
          , ""
          , "Potential problems could be:"
          , "  * Misspelled the module name"
          , "  * Need to add a source directory or new dependency to " ++ Paths.description
          ]

    ImportFoundTooMany name maybeParent filePaths pkgs ->
        "Error when searching for modules" ++ toContext maybeParent ++ ".\n"
        ++ "Found multiple modules named '" ++ Module.nameToString name ++ "'\n"
        ++ "Modules with that name were found in the following locations:\n"
        ++ "\n"
        ++ concatMap (\str -> "    " ++ str ++ "\n") (paths ++ packages)
      where
        packages =
            map ("package " ++) (map Pkg.toString pkgs)

        paths =
            map ("directory " ++) filePaths

    ModuleNameFileNameMismatch path nameFromPath nameFromSource ->
        unlines
          [ "The module name is messed up for " ++ path
          , ""
          , "    According to the file's name it should be " ++ Module.nameToString nameFromPath
          , "    According to the source code it should be " ++ Module.nameToString nameFromSource
          , ""
          , "Which is it?"
          ]


toContext :: Maybe Module.Name -> String
toContext maybeParent =
  case maybeParent of
    Nothing ->
        " exposed by " ++ Paths.description

    Just parent ->
        " imported by module '" ++ Module.nameToString parent ++ "'"
