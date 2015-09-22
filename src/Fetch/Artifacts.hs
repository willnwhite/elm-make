{-# OPTIONS_GHC -Wall #-}
module Fetch.Artifacts
    ( getSolvedDependencies
    , putSolvedDependencies
    , sourceCodeDownloaded
    , unzipSourceCode
    , removeSourceCode
    , getDescription
    , putDescription
    , getString
    , putString
    )
    where

import qualified Codec.Archive.Zip as Zip
import Control.Monad.Except (liftIO)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Elm.Compiler as Compiler
import Elm.Compiler.Module as Module
import Elm.Package as Pkg
import GHC.IO.Exception ( IOErrorType(InvalidArgument) )
import System.Directory (doesDirectoryExist, doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>), (<.>))
import qualified System.IO as IO
import qualified System.IO.Error as IOError

import qualified Dependency.Solution as Solution
import qualified Fetch.Artifacts.Paths as Paths
import qualified Package.Description as Desc
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


-- DESCRIPTIONS

getDescription :: Maybe (Pkg.Name, Pkg.Version) -> Task.Task Desc.Description
getDescription maybePackage =
  let
    path =
      case maybePackage of
        Nothing ->
            Paths.description

        Just (name, vsn) ->
            sourceCodeDir name vsn </> Paths.description
  in
    do  json <- liftIO (BS.readFile path)
        case Json.eitherDecode json of
          Left err ->
              Task.throw $ error "TODO" $ "Error reading file " ++ path ++ ":\n    " ++ err

          Right description ->
              return description


putDescription :: Desc.Description -> IO ()
putDescription description =
  BS.writeFile Paths.description (Desc.prettyJSON description)


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


-- GET AND PUT UTF8 FILES

{-| getString converts Text to String instead of reading a String directly
because System.IO.hGetContents is lazy, and with lazy IO, decoding exception
cannot be caught. By using the strict Text type, we force any decoding
exceptions to be thrown so we can show our UTF-8 message.
-}
getString :: FilePath -> Task.Task String
getString name =
  do  text <- liftIO (getText name)
      return (Text.unpack text)


-- TODO - this should return a Task that can fail with a nice message through
-- the normal error reporting mechanisms.
getText :: FilePath -> IO Text.Text
getText name =
  let
    callback handle =
      IOError.modifyIOError (convertUtf8Error name) (TextIO.hGetContents handle)
  in
    withFileUtf8 name IO.ReadMode callback


convertUtf8Error :: FilePath -> IOError -> IOError
convertUtf8Error filepath e =
  case IOError.ioeGetErrorType e of
    InvalidArgument ->
        IOError.annotateIOError (userError "Bad encoding; the file must be valid UTF-8") "" Nothing (Just filepath)

    _ ->
        e


putString :: FilePath -> String -> IO ()
putString filePath str =
  putText filePath (Text.pack str)


putText :: FilePath -> Text.Text -> IO ()
putText filePath txt =
  withFileUtf8 filePath IO.WriteMode (\handle -> TextIO.hPutStr handle txt)


withFileUtf8 :: FilePath -> IO.IOMode -> (IO.Handle -> IO a) -> IO a
withFileUtf8 filePath mode callback =
  IO.withFile filePath mode $ \handle ->
      do  IO.hSetEncoding handle IO.utf8
          callback handle
