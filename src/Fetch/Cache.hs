module Fetch.Cache
    ( getSourceCode
    , putSourceCode
    , getAvailablePackagesModificationTime
    , getAvailablePackages
    , putAvailablePackages
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Data.Time.Clock as Time
import qualified Elm.Package as Pkg
import qualified System.Directory as Dir
import System.FilePath ((</>), (<.>))
import System.IO (hFileSize, withBinaryFile, IOMode(WriteMode))


getDataDir :: IO FilePath
getDataDir =
  do  root <- Dir.getAppUserDataDirectory "elm"
      Dir.createDirectoryIfMissing True root
      return root


-- SOURCE CODE CACHE MANAGEMENT

maxCacheBytes :: Integer
maxCacheBytes =
    5 * 1024 * 1024


data CacheEntry = CacheEntry
    { _name :: Pkg.Name
    , _version :: Pkg.Version
    , _bytes :: Integer
    }


entryToPath :: FilePath -> CacheEntry -> FilePath
entryToPath dataDir (CacheEntry name version _) =
  sourceCodePath dataDir name version


instance Binary.Binary CacheEntry where
    get =
        CacheEntry <$> Binary.get <*> Binary.get <*> Binary.get

    put (CacheEntry name version bytes) =
        do  Binary.put name
            Binary.put version
            Binary.put bytes


insert :: FilePath -> CacheEntry -> [CacheEntry] -> ([CacheEntry], [FilePath])
insert dataDir entry@(CacheEntry _ _ bytes) entries =
  first (entry:) (insertHelp dataDir bytes entries)


insertHelp :: FilePath -> Integer -> [CacheEntry] -> ([CacheEntry], [FilePath])
insertHelp dataDir totalBytes entries =
  case entries of
    [] ->
        ([], [])

    entry@(CacheEntry _ _ bytes) : remaining ->
        let
          newTotal = totalBytes + bytes
        in
          if newTotal > maxCacheBytes then
              ([], map (entryToPath dataDir) entries)

          else
              first (entry:) (insertHelp dataDir newTotal remaining)


touch :: FilePath -> Pkg.Name -> Pkg.Version -> [CacheEntry] -> ([CacheEntry], Maybe FilePath)
touch dataDir name version entries =
  case touchHelp name version entries of
    Nothing ->
        (entries, Nothing)

    Just (entries, entry) ->
        (entry : entries, Just (sourceCodePath dataDir name version))


touchHelp :: Pkg.Name -> Pkg.Version -> [CacheEntry] -> Maybe ([CacheEntry], CacheEntry)
touchHelp name version entries =
  case entries of
    [] ->
        Nothing

    entry@(CacheEntry entryName entryVersion _) : remaining ->
        if name == entryName && version == entryVersion then
            Just (remaining, entry)

        else
            first (entry:) <$> touchHelp name version remaining


-- GET AND PUT SOURCE CODE

sourceCodePath :: FilePath -> Pkg.Name -> Pkg.Version -> FilePath
sourceCodePath dataDir name version =
    dataDir </> "cache" </> Pkg.toFilePath name </> Pkg.versionToString version <.> "zip"


cacheInfoPath :: FilePath -> FilePath
cacheInfoPath dataDir =
    dataDir </> "cache" </> "source-code-cache-info" <.> "dat"


putSourceCode :: Pkg.Name -> Pkg.Version -> BS.ByteString -> IO ()
putSourceCode name version bytes =
  do  dataDir <- getDataDir

      bytes <-
          withBinaryFile (sourceCodePath dataDir name version) WriteMode $ \handle ->
            do  BS.hPut handle bytes
                hFileSize handle

      cacheInfo <-
          Binary.decodeFile (cacheInfoPath dataDir)

      let (newCacheInfo, evictions) =
            insert dataDir (CacheEntry name version bytes) cacheInfo

      mapM_ Dir.removeFile evictions

      Binary.encodeFile (cacheInfoPath dataDir) newCacheInfo


getSourceCode :: Pkg.Name -> Pkg.Version -> IO (Maybe FilePath)
getSourceCode name version =
  do  dataDir <- getDataDir

      cacheInfo <- Binary.decodeFile (cacheInfoPath dataDir)

      let (newCacheInfo, maybeZipPath) =
            touch dataDir name version cacheInfo

      Binary.encodeFile (cacheInfoPath dataDir) newCacheInfo

      return maybeZipPath


-- ALL AVAILABLE PACKAGES

versionsPath :: FilePath -> FilePath
versionsPath dataDir =
  dataDir </> "cache" </> "versions.dat"


getAvailablePackagesModificationTime :: IO (Maybe Time.UTCTime)
getAvailablePackagesModificationTime =
  do  dataDir <- getDataDir
      exists <- Dir.doesFileExist (versionsPath dataDir)
      if exists
        then
          Just <$> Dir.getModificationTime (versionsPath dataDir)

        else
          return Nothing


getAvailablePackages :: IO (Map.Map Pkg.Name [Pkg.Version])
getAvailablePackages =
  do  dataDir <- getDataDir
      Binary.decodeFile (versionsPath dataDir)


putAvailablePackages :: Map.Map Pkg.Name [Pkg.Version] -> IO ()
putAvailablePackages packages =
  do  dataDir <- getDataDir
      Binary.encodeFile (versionsPath dataDir) packages
