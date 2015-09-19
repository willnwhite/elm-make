{-# OPTIONS_GHC -Wall #-}
module Dependency.Solution
    ( Solution
    , toJson
    , fromJson
    )
    where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Elm.Package as Pkg


type Solution =
    Map.Map Pkg.Name Pkg.Version


-- CONVERSION TO JSON

toJson :: Solution -> BS.ByteString
toJson solution =
    encodePretty (object (map toField (Map.toList solution)))
  where
    toField (name, version) =
        Text.pack (Pkg.toString name) .= Text.pack (Pkg.versionToString version)


fromJson :: BS.ByteString -> Either String Solution
fromJson rawJson =
  do  hashMap <- eitherDecode rawJson
      pairs <- mapM parseNameAndVersion (HashMap.toList hashMap)
      return (Map.fromList pairs)


parseNameAndVersion :: (String,String) -> Either String (Pkg.Name, Pkg.Version)
parseNameAndVersion (rawName, rawVersion) =
    do  name <- parse rawName Pkg.fromString ("package name " ++ rawName)
        vrsn <- parse rawVersion Pkg.versionFromString ("version number for package " ++ rawName)
        return (name, vrsn)


parse :: String -> (String -> Maybe a) -> String -> Either String a
parse string fromString msg =
    maybe (Left ("Could not parse " ++ msg)) return (fromString string)
