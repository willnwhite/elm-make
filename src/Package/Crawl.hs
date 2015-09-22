{-# OPTIONS_GHC -Wall #-}
module Package.Crawl (fromFiles, fromDescription) where

import Control.Monad (when)
import Control.Monad.Trans (lift, liftIO)
import qualified Control.Monad.State as State
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg
import System.Directory (doesFileExist)
import System.FilePath ((</>), (<.>))

import qualified Fetch.Artifacts as Artifacts
import qualified Package.Description as Desc
import qualified Dependency.Solution as S
import qualified TheMasterPlan as TMP
import TheMasterPlan ( CanonicalModule(CanonicalModule), ModuleGraph, ModuleInfo(..), Location(Location) )
import qualified Task
import qualified Task.Error as Error


-- STATE and ENVIRONMENT

type Explorer =
    State.StateT ModuleGraph Task.Task


throw :: Error.Error -> Explorer a
throw err =
  lift (Task.throw err)


data Env =
  Env
    { _package :: TMP.Package
    , _allowNatives :: Bool
    , _sourceDirs :: [FilePath]
    , _exposedModulesInDependencies :: Map.Map Module.Name [TMP.Package]
    }


initEnv :: FilePath -> Desc.Description -> S.Solution -> Task.Task Env
initEnv root desc solution =
  do  exposedModules <- getExposedModulesInDependencies desc solution
      return $ Env
          { _package = (Desc.name desc, Desc.version desc)
          , _allowNatives = Desc.natives desc
          , _sourceDirs = map (root </>) (Desc.sourceDirs desc)
          , _exposedModulesInDependencies = exposedModules
          }


-- GENERIC EXPLORERS

fromFiles :: FilePath -> S.Solution -> Desc.Description -> [FilePath] -> Task.Task ([CanonicalModule], ModuleGraph)
fromFiles root solution desc filePaths =
  do  env <- initEnv root desc solution
      State.runStateT (mapM (exploreFile env []) filePaths) Map.empty


fromDescription :: FilePath -> S.Solution -> Desc.Description -> Task.Task ModuleGraph
fromDescription root solution desc =
  do  env <- initEnv root desc solution
      State.execStateT (mapM_ (exploreModule env []) (Desc.exposed desc)) Map.empty



-- EXPLORE A SOURCE FILE

exploreFile :: Env -> [Module.Name] -> FilePath -> Explorer CanonicalModule
exploreFile env@(Env pkg _ _ _) parents filePath =
  do  -- explore the source code
      sourceCode <- lift $ Artifacts.getString filePath

      (givenName, givenImports) <-
          case Compiler.parseDependencies sourceCode of
            Right result ->
                return result

            Left msgs ->
                throw (error "TODO") -- (Error.CompilerErrors filePath sourceCode msgs)

      -- verify name
      checkName filePath givenName (Maybe.listToMaybe parents)
      let name = CanonicalModule pkg givenName

      -- verify imports
      let rawImports =
            (if fst pkg == Pkg.coreName then id else (Module.defaultImports ++)) givenImports

      imports <- mapM (exploreModule env (givenName : parents)) rawImports

      -- inform people about the results
      State.modify $ Map.insert name (Elm (Location filePath pkg) imports)
      return name


checkName :: FilePath -> Module.Name -> Maybe Module.Name -> Explorer ()
checkName path givenName maybeExpectedName =
    case maybeExpectedName of
      Nothing ->
          return ()

      Just expectedName ->
          if givenName == expectedName then
              return ()

          else
              throw (Error.ModuleNameFileNameMismatch path expectedName givenName)


-- EXPLORE A MODULE

exploreModule :: Env -> [Module.Name] -> Module.Name -> Explorer CanonicalModule
exploreModule env@(Env pkg _ _ exposedModules) parents moduleName =
  do  when (elem moduleName parents) $
          throw (Error.ImportCycle (dropWhile (/=moduleName) (reverse parents)))

      codePaths <- findAllPaths env moduleName

      case (codePaths, Map.lookup moduleName exposedModules) of
        ([], Nothing) ->
            throw (Error.ImportNotFound moduleName (Maybe.listToMaybe parents))

        ([], Just [subPkg]) ->
            return (CanonicalModule subPkg moduleName)

        ([ElmPath filePath], Nothing) ->
            exploreFile env parents filePath

        ([JsPath filePath], Nothing) ->
            do  let canonicalName = CanonicalModule pkg moduleName
                State.modify (Map.insert canonicalName (JS (Location filePath pkg)))
                return canonicalName

        (_, maybePkgs) ->
            throw $
              Error.ImportFoundTooMany
                moduleName
                (Maybe.listToMaybe parents)
                (map toFilePath codePaths)
                (maybe [] (map fst) maybePkgs)


data CodePath
    = ElmPath FilePath
    | JsPath FilePath


toFilePath :: CodePath -> FilePath
toFilePath codePath =
  case codePath of
    ElmPath file ->
        file

    JsPath file ->
        file


findAllPaths :: Env -> Module.Name -> Explorer [CodePath]
findAllPaths (Env _ allowNatives sourceDirs _) moduleName =
    findHelp allowNatives moduleName sourceDirs []


findHelp :: Bool -> Module.Name -> [FilePath] -> [CodePath] -> Explorer [CodePath]
findHelp allowNatives moduleName sourceDirs foundPaths =
  case sourceDirs of
    [] ->
        return foundPaths

    dir : srcDirs ->
        do  newFoundPaths <-
                case moduleName of
                  Module.Name ("Native" : _) ->
                      if allowNatives then
                          addJsPath dir moduleName foundPaths
                      else
                          return foundPaths

                  _ ->
                      addElmPath dir moduleName foundPaths

            findHelp allowNatives moduleName srcDirs newFoundPaths


consIf :: Bool -> a -> [a] -> [a]
consIf bool x xs =
  if bool then x:xs else xs


addElmPath :: FilePath -> Module.Name -> [CodePath] -> Explorer [CodePath]
addElmPath dir moduleName locs =
  do  let elmPath = dir </> Module.nameToPath moduleName <.> "elm"
      elmExists <- liftIO (doesFileExist elmPath)
      return (consIf elmExists (ElmPath elmPath) locs)


addJsPath :: FilePath -> Module.Name -> [CodePath] -> Explorer [CodePath]
addJsPath dir moduleName locs =
  do  let jsPath = dir </> Module.nameToPath moduleName <.> "js"
      jsExists <- liftIO (doesFileExist jsPath)
      return (consIf jsExists (JsPath jsPath) locs)


-- EXPOSED MODULES in DEPENDENCIES

getExposedModulesInDependencies
    :: Desc.Description
    -> S.Solution
    -> Task.Task (Map.Map Module.Name [TMP.Package])
getExposedModulesInDependencies desc solution =
  do  visibleDependencies <- allVisibleDependencies desc solution
      rawLocations <- mapM getExposedModules visibleDependencies
      return (Map.unionsWith (++) rawLocations)


allVisibleDependencies :: Desc.Description -> S.Solution -> Task.Task [(Pkg.Name, Pkg.Version)]
allVisibleDependencies desc solution =
  let
    visible =
      map fst (Desc.dependencies desc)

    getVersion name =
      case Map.lookup name solution of
        Just version ->
            return (name, version)

        Nothing ->
            Task.throw (error "TODO") -- Error.MissingPackage name
  in
    mapM getVersion visible


getExposedModules
    :: (Pkg.Name, Pkg.Version)
    -> Task.Task (Map.Map Module.Name [(Pkg.Name, Pkg.Version)])
getExposedModules packageID =
  let
    insert dict moduleName =
      Map.insert moduleName [packageID] dict
  in
    do  description <- Artifacts.getDescription (Just packageID)
        return (List.foldl' insert Map.empty (Desc.exposed description))
