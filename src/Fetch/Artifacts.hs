{-# OPTIONS_GHC -Wall #-}
module Fetch.Artifacts
    ( getSolvedDependencies
    , putSolvedDependencies
    , sourceCodeDownloaded
    , unzipSourceCode
    , removeSourceCode
    )
    where

import qualified Codec.Archive.Zip as Zip
import Control.Monad.Except (liftIO)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Elm.Compiler as Compiler
import Elm.Compiler.Module as Module
import Elm.Package as Pkg
import System.Directory (doesDirectoryExist, doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>), (<.>))

import qualified Dependency.Solution as Solution
import qualified Fetch.Artifacts.Paths as Paths
import qualified Task
import qualified TheMasterPlan as TMP


-- SOLVED DEPENDENCIES
--
-- Describes the exact versions of every package used for your project. This
-- information is written by elm-package when it solves and installs
-- dependencies.

solvedDependenciesPath :: FilePath
solvedDependenciesPath =
    Paths.stuffDirectory </> "exact-dependencies.json"


getSolvedDependencies :: Task.Task Solution.Solution
getSolvedDependencies =
  do  exists <- liftIO (doesFileExist solvedDependenciesPath)
      if exists
        then
          do  bytes <- liftIO (BS.readFile solvedDependenciesPath)
              either (Task.throw . error "TODO") return (Solution.fromJson bytes)

        else
          return Map.empty


putSolvedDependencies :: Solution.Solution -> IO ()
putSolvedDependencies solution =
  BS.writeFile solvedDependenciesPath (Solution.toJson solution)


-- SOURCE CODE PATHS

packagesDirectory :: FilePath
packagesDirectory =
    Paths.stuffDirectory </> "packages"


sourceCodeDir :: Pkg.Name -> Pkg.Version -> FilePath
sourceCodeDir name version =
    packagesDirectory </> Pkg.toFilePath name </> Pkg.versionToString version


sourceCodeDownloaded :: Pkg.Name -> Pkg.Version -> IO Bool
sourceCodeDownloaded name version =
    doesDirectoryExist (sourceCodeDir name version)


unzipSourceCode :: Pkg.Name -> Pkg.Version -> BS.ByteString -> IO ()
unzipSourceCode name version bytes =
    Zip.extractFilesFromArchive
      [Zip.OptLocation (sourceCodeDir name version) False]
      (Zip.toArchive bytes)


removeSourceCode :: Pkg.Name -> Pkg.Version -> IO ()
removeSourceCode name version =
    removeDirectoryRecursive (sourceCodeDir name version)


-- BUILD ARTIFACTS

artifactDirectory :: FilePath
artifactDirectory =
    Paths.stuffDirectory </> "build-artifacts" </> Pkg.versionToString Compiler.version


toInterface :: TMP.CanonicalModule -> FilePath
toInterface (TMP.CanonicalModule pkg name) =
    artifactDirectory </> inPackage pkg (Module.hyphenate name <.> "elmi")


toObjectFile :: TMP.CanonicalModule -> FilePath
toObjectFile (TMP.CanonicalModule pkg name) =
    artifactDirectory </> inPackage pkg (Module.hyphenate name <.> "elmo")


toPackageCacheFile :: TMP.Package -> FilePath
toPackageCacheFile pkg =
    artifactDirectory </> inPackage pkg "graph.dat"


inPackage :: TMP.Package -> FilePath -> FilePath
inPackage (name, version) relativePath =
    Pkg.toFilePath name </> Pkg.versionToString version </> relativePath


-- SOURCE FILES

toSource :: TMP.Location -> FilePath
toSource (TMP.Location relativePath _package) =
    relativePath
