{-# OPTIONS_GHC -Wall #-}
module Flags
    ( parser
    , Flags(..)
    , Output(..), outputFilePath
    , ReportType(..)
    ) where

import Control.Applicative ((<$>), (<*>), many, optional)
import Control.Monad.Except (liftIO)
import qualified Data.List as List
import Data.Monoid ((<>), mconcat, mempty)
import Data.Version (showVersion)
import qualified Elm.Compiler as Compiler
import qualified Elm.Package as Pkg
import qualified Options.Applicative as O
import qualified Options.Applicative.Help.Chunk as Chunk
import System.FilePath (takeExtension)


-- FLAGS

data Flags = Flags
    { _files :: [FilePath]
    , _output :: Output
    , _autoYes :: Bool
    , _reportType :: ReportType
    , _warn :: Bool
    , _docs :: Maybe FilePath
    }


data Output
    = Html FilePath
    | JS FilePath


data ReportType = Normal | Json


outputFilePath :: Flags -> FilePath
outputFilePath config =
  case _output config of
    Html file -> file
    JS file -> file


-- PARSE ARGUMENTS

parser :: ParserInfo Flags
parser =
  ParserInfo
    { infoParser =
        flagParser

    , infoFullDesc =
        True

    , infoProgDesc =
        Chunk.paragraph "build Elm projects"

    , infoHeader =
        Chunk.string ("elm make - part of elm " ++ Pkg.versionToString Compiler.version)

    , infoFooter =
        Chunk.vcatChunks $ map Chunk.string $
            [ "Examples:"
            , "  elm make Main.elm                     # compile to HTML in index.html"
            , "  elm make Main.elm --output main.html  # compile to HTML in main.html"
            , "  elm make Main.elm --output elm.js     # compile to JS in elm.js"
            , "  elm make Main.elm --warn              # compile and report warnings"
            , ""
            , "Full guide to using elm-make at <https://github.com/elm-lang/elm-make>"
            ]

    , infoFailureCode =
        1

    , infoIntersperse =
        True
    }


-- ACTUALLY PARSE FLAGS

flagParser :: O.Parser Flags
flagParser =
    Flags
      <$> files
      <*> output
      <*> yes
      <*> report
      <*> warn
      <*> optional docs


files :: O.ParserInfo [String]
files =
    many (O.strArgument ( O.metavar "FILES..." ))


output :: O.Parser Output
output =
    O.option outputReader $
      mconcat
        [ O.long "output"
        , O.metavar "FILE"
        , O.value (Html "index.html")
        , O.help "Write result to the given .html or .js FILE."
        ]


yes :: O.Parser Bool
yes =
    O.switch $
      mconcat
        [ O.long "yes"
        , O.help "Reply 'yes' to all automated prompts."
        ]


report :: O.Parser ReportType
report =
    O.option reportReader $
      mconcat
        [ O.long "report"
        , O.metavar "FORMAT"
        , O.value Normal
        , O.help "Format of error and warning reports (e.g. --report=json)"
        ]


warn :: O.Parser Bool
warn =
    O.switch $
      mconcat
        [ O.long "warn"
        , O.help "Report warnings to improve code quality."
        ]


docs :: O.Parser FilePath
docs =
    O.strOption $
      mconcat
        [ O.long "docs"
        , O.metavar "FILE"
        , O.help "Write documentation to FILE as JSON."
        ]


-- FANCY READERS

customReader :: [String] -> (String -> Maybe a) -> O.ReadM a
customReader options fromString =
  let reader arg =
        case fromString arg of
          Just a ->
              Right a

          Nothing ->
              Left $
                "acceptable arguments to this flag include: "
                ++ List.intercalate ", " options
  in
      O.eitherReader reader


outputReader :: O.ReadM Output
outputReader =
    customReader ["any file that ends with .html or .js"] $
        \path ->
            let
                ext =
                    takeExtension path
            in
                if ext == ".html" then
                    Just (Html path)

                else if ext == ".js" then
                    Just (JS path)

                else
                    Nothing


reportReader :: O.ReadM ReportType
reportReader =
    customReader ["normal","json"] $
        \str ->
            case str of
                "normal" ->
                    Just Normal

                "json" ->
                    Just Json

                _ ->
                    Nothing
