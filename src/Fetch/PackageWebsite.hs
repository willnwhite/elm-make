{-# LANGUAGE OverloadedStrings #-}
module Fetch.PackageWebsite where

import Control.Applicative ((<$>))
import Control.Monad.Except (liftIO)
import Data.Aeson ((.:))
import qualified Data.Aeson as Json
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Network.HTTP
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.MultipartFormData as Multi
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), dropFileName)

import qualified Elm.Compiler as Compiler
import qualified Elm.Docs as Docs
import qualified Elm.Package as Pkg
import qualified Fetch.Artifacts.Paths as Paths
import qualified Package.Description as Desc
import qualified Fetch.Cache as Cache
import qualified Fetch.HttpHelpers as Http
import qualified Task


-- MAKE URLs THAT POINT TO THE WEBSITE

toUrl :: String -> [(String,String)] -> String
toUrl path vars =
  let
    version =
      ("elm-package-version", Pkg.versionToString Compiler.version)
  in
    "http://package.elm-lang.org/"
    ++ path
    ++ "?"
    ++ urlEncodeVars (version : vars)


-- GET ALL PUBLISHED VERSIONS OF A PACKAGE

versions :: Pkg.Name -> Task.Task [Pkg.Version]
versions name =
  do  maybeVersions <-
          Http.send (toUrl "versions" [("name", Pkg.toString name)]) $
            \request manager ->
                do  response <- Client.httpLbs request manager
                    return $ Binary.decode $ Client.responseBody response

      case maybeVersions of
        Nothing ->
            Task.throw (error "TODO")

        Just versions ->
            return versions


-- GET THE FULL PACKAGE LISTING

allPackages :: Task.Task (Map.Map Pkg.Name [Pkg.Version])
allPackages =
  do  maybeTime <-
          liftIO Cache.getAvailablePackagesModificationTime

      let vars =
            maybe [] (\time -> [("since", show time)]) maybeTime

      maybePackageList <-
          Http.send (toUrl "all-packages" vars) $
            \request manager ->
                do  response <- Client.httpLbs request manager
                    let decodedValue = Json.decode (Client.responseBody response)
                    return (map (\(PackageSummary s) -> s) <$> decodedValue)

      case (maybeTime, maybePackageList) of
        (Nothing, Nothing) ->
            error "TODO"

        (Just _, Nothing) ->
            liftIO Cache.getAvailablePackages

        (_, Just packageList) ->
            let
              pacakges =
                Map.fromList packageList
            in
              do  liftIO (Cache.putAvailablePackages pacakges)
                  return pacakges


newtype PackageSummary =
    PackageSummary (Pkg.Name, [Pkg.Version])


instance Json.FromJSON PackageSummary where
    parseJSON (Json.Object obj) =
      do  name <- obj .: "name"
          versions <- obj .: "versions"
          return (PackageSummary (name, versions))

    parseJSON _ =
      fail "package summary must be an object"


-- REGISTER YOUR PACKAGE

register :: Pkg.Name -> Pkg.Version -> Task.Task ()
register name version =
  let
    vars =
        [ ("name", Pkg.toString name)
        , ("version", Pkg.versionToString version)
        ]

    files =
        [ Multi.partFileSource "documentation" Paths.documentation
        , Multi.partFileSource "description" Paths.description
        , Multi.partFileSource "readme" "README.md"
        ]
  in
    Http.send (toUrl "register" vars) $
      \request manager ->
          do  request' <- Multi.formDataBody files request
              let request'' = request' { Client.responseTimeout = Nothing }
              Client.httpLbs request'' manager
              return ()


-- GET VARIOUS INFORMATION ABOUT A PACKAGE

description :: Pkg.Name -> Pkg.Version -> Task.Task Desc.Description
description name version =
  getJson "description" Paths.description name version


documentation :: Pkg.Name -> Pkg.Version -> Task.Task [Docs.Documentation]
documentation name version =
  getJson "documentation" "documentation.json" name version


getJson :: (Json.FromJSON a) => String -> FilePath -> Pkg.Name -> Pkg.Version -> Task.Task a
getJson metadata metadataPath name version =
  do  cacheDir <- error "asks Manager.cacheDirectory"
      let fullMetadataPath =
            cacheDir </> Pkg.toFilePath name </> Pkg.versionToString version </> metadataPath

      exists <- liftIO (doesFileExist fullMetadataPath)

      content <-
        case exists of
          True ->
            liftIO (LBS.readFile fullMetadataPath)

          False ->
            let
              vars =
                [ ("name", Pkg.toString name)
                , ("version", Pkg.versionToString version)
                ]
            in
              Http.send (toUrl metadata vars) $
                \request manager ->
                  do  response <- Client.httpLbs request manager
                      createDirectoryIfMissing True (dropFileName fullMetadataPath)
                      LBS.writeFile fullMetadataPath (Client.responseBody response)
                      return (Client.responseBody response)

      case Json.eitherDecode content of
        Right value ->
            return value

        Left err ->
            Task.throw $ error "TODO" $
                "Unable to get " ++ metadataPath ++ " for "
                ++ Pkg.toString name ++ " " ++ Pkg.versionToString version ++ "\n" ++ err
