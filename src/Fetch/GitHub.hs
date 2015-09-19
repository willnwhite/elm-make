{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Fetch.GitHub
    ( getSourceCode
    , getVersionTags
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Except (liftIO, when)
import Data.Aeson as Json
import qualified Data.ByteString.Lazy as BS
import qualified Data.Maybe as Maybe
import qualified Data.Vector as Vector
import qualified Data.List as List
import qualified Network.HTTP.Client as Client
import System.Directory (getDirectoryContents, renameDirectory)

import qualified Elm.Package as Pkg
import qualified Fetch.Artifacts as Artifacts
import qualified Fetch.Cache as Cache
import qualified Fetch.HttpHelpers as Http
import qualified Task


-- FETCH SOURCE CODE FROM GITHUB

getSourceCode :: Pkg.Name -> Pkg.Version -> Task.Task ()
getSourceCode name@(Pkg.Name user _) version =
  do  downloaded <- liftIO $ Artifacts.sourceCodeDownloaded name version
      when (not downloaded) $
        do  maybeZipPath <- liftIO $ Cache.getSourceCode name version
            case maybeZipPath of
              Just zipPath ->
                  liftIO (Artifacts.unzipSourceCode name version =<< BS.readFile zipPath)

              Nothing ->
                  do  Http.send (zipUrl name version) (extract name version)
                      files <- liftIO $ getDirectoryContents "."
                      case List.find (List.isPrefixOf user) files of
                        Nothing ->
                            Task.throw (error "TODO")

                        Just dir ->
                            liftIO $ renameDirectory dir (Pkg.versionToString version)


extract :: Pkg.Name -> Pkg.Version -> Client.Request -> Client.Manager -> IO ()
extract name version request manager =
  do  bytes <- Client.responseBody <$> Client.httpLbs request manager
      Cache.putSourceCode name version bytes
      Artifacts.unzipSourceCode name version bytes


zipUrl :: Pkg.Name -> Pkg.Version -> String
zipUrl name version =
  "http://github.com/" ++ Pkg.toUrl name ++ "/zipball/" ++ Pkg.versionToString version ++ "/"


-- TAGS FROM GITHUB

getVersionTags :: Pkg.Name -> Task.Task [Pkg.Version]
getVersionTags (Pkg.Name user project) =
  do  response <-
          Http.send url $ \request manager ->
              Client.httpLbs (request { Client.requestHeaders = headers }) manager

      case Json.eitherDecode (Client.responseBody response) of
        Left err ->
            Task.throw (error "TODO")

        Right (Tags tags) ->
            return (Maybe.mapMaybe Pkg.versionFromString tags)

  where
    url =
      "https://api.github.com/repos/" ++ user ++ "/" ++ project ++ "/tags"

    headers =
      [ ("User-Agent", "elm-package")
      , ("Accept", "application/json")
      ]


newtype Tags = Tags [String]


instance Json.FromJSON Tags where
    parseJSON (Json.Array arr) =
        let
            toTag (Json.Object obj) = obj .: "name"
            toTag _ = fail "expecting an object"
        in
            Tags `fmap` mapM toTag (Vector.toList arr)

    parseJSON _ =
        fail "expecting an array"

