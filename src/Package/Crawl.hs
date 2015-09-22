{-# OPTIONS_GHC -Wall #-}
module Package.Crawl (fromFiles, fromExposedModules) where

import Control.Arrow (second)
import Control.Monad.Except (liftIO)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg
import System.Directory (doesFileExist)
import System.FilePath ((</>), (<.>))

import qualified Fetch.Artifacts as Artifacts
import qualified Package.Description as Desc
import qualified Dependency.Solution as S
import qualified TheMasterPlan as TMP
import TheMasterPlan ( PackageGraph(..), PackageData(..) )
import qualified Task
import qualified Task.Error as Error


-- STATE and ENVIRONMENT

data Env =
  Env
    { _packageName :: Pkg.Name
    , _allowNatives :: Bool
    , _sourceDirs :: [FilePath]
    , _exposedModulesInDependencies :: Map.Map Module.Name [(Pkg.Name, Pkg.Version)]
    }


initEnv :: FilePath -> Desc.Description -> S.Solution -> Task.Task Env
initEnv root desc solution =
  do  exposedModules <- getExposedModulesInDependencies desc solution
      return $ Env
          { _packageName = Desc.name desc
          , _allowNatives = Desc.natives desc
          , _sourceDirs = map (root </>) (Desc.sourceDirs desc)
          , _exposedModulesInDependencies = exposedModules
          }


-- GENERIC CRAWLER

fromFiles
    :: FilePath
    -> S.Solution
    -> Desc.Description
    -> [FilePath]
    -> Task.Task ([Module.Name], PackageGraph)
fromFiles root solution desc filePaths =
  do  env <- initEnv root desc solution

      let pkgName = Desc.name desc
      info <- mapM (getFileInfo pkgName Nothing) filePaths
      let names = map fst info
      let unvisited = concatMap (snd . snd) info
      let pkgData = Map.fromList (map (second fst) info)
      let initialGraph = PackageGraph pkgData Map.empty Map.empty

      graph <- dfs env unvisited initialGraph

      return (names, graph)


fromExposedModules
    :: FilePath
    -> S.Solution
    -> Desc.Description
    -> Task.Task PackageGraph
fromExposedModules root solution desc =
  do  env <- initEnv root desc solution
      let unvisited = addParent Nothing (Desc.exposed desc)
      let initialGraph = PackageGraph Map.empty Map.empty Map.empty
      dfs env unvisited initialGraph


-- DEPTH FIRST SEARCH

dfs
    :: Env
    -> [(Module.Name, Maybe Module.Name)]
    -> PackageGraph
    -> Task.Task PackageGraph
dfs env visitList graph =
  case visitList of
    [] ->
        return graph

    (name, maybeParent) : remaining ->
        if Map.member name (packageData graph) then
            dfs env remaining graph

        else
            dfsHelp env name maybeParent remaining graph


dfsHelp
    :: Env
    -> Module.Name
    -> Maybe Module.Name
    -> [(Module.Name, Maybe Module.Name)]
    -> PackageGraph
    -> Task.Task PackageGraph
dfsHelp env moduleName maybeParent unvisited graph =
  do
      codePaths <- find env moduleName

      case (codePaths, Map.lookup moduleName (_exposedModulesInDependencies env)) of
        ([Elm filePath], Nothing) ->
            do  (name, (pkgData, newUnvisited)) <-
                    getFileInfo (_packageName env) (Just moduleName) filePath

                dfs env (newUnvisited ++ unvisited) $ graph {
                    packageData = Map.insert name pkgData (packageData graph)
                }

        ([JS filePath], Nothing) ->
            dfs env unvisited $ graph {
                packageNatives =
                    Map.insert moduleName filePath (packageNatives graph)
            }

        ([], Just [pkg]) ->
            dfs env unvisited $ graph {
                packageForeignDependencies =
                    Map.insert moduleName pkg (packageForeignDependencies graph)
            }

        ([], Nothing) ->
            Task.throw (Error.ImportNotFound moduleName maybeParent)

        (_, maybePkgs) ->
            Task.throw $
              Error.ImportFoundTooMany
                moduleName
                maybeParent
                (map toFilePath codePaths)
                (maybe [] (map fst) maybePkgs)


-- FIND LOCAL FILE PATH

data CodePath = Elm FilePath | JS FilePath


toFilePath :: CodePath -> FilePath
toFilePath codePath =
  case codePath of
    Elm file -> file
    JS file -> file


find :: Env -> Module.Name -> Task.Task [CodePath]
find (Env _ allowNatives sourceDirs _) moduleName =
    findHelp allowNatives moduleName sourceDirs []


findHelp :: Bool -> Module.Name -> [FilePath] -> [CodePath] -> Task.Task [CodePath]
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


addElmPath :: FilePath -> Module.Name -> [CodePath] -> Task.Task [CodePath]
addElmPath dir moduleName locs =
  do  let elmPath = dir </> Module.nameToPath moduleName <.> "elm"
      elmExists <- liftIO (doesFileExist elmPath)
      return (consIf elmExists (Elm elmPath) locs)


addJsPath :: FilePath -> Module.Name -> [CodePath] -> Task.Task [CodePath]
addJsPath dir moduleName locs =
  do  let jsPath = dir </> Module.nameToPath moduleName <.> "js"
      jsExists <- liftIO (doesFileExist jsPath)
      return (consIf jsExists (JS jsPath) locs)



-- GET INFO FROM A SOURCE FILE

getFileInfo
    :: Pkg.Name
    -> Maybe Module.Name
    -> FilePath
    -> Task.Task (Module.Name, (PackageData, [(Module.Name, Maybe Module.Name)]))
getFileInfo pkgName maybeName filePath =
  do  sourceCode <- Artifacts.getString filePath

      (name, rawDeps) <-
          case Compiler.parseDependencies sourceCode of
            Right result ->
                return result

            Left msgs ->
                Task.throw (error "TODO") -- (Error.CompilerErrors filePath sourceCode msgs)

      checkName filePath name maybeName

      let deps =
            if pkgName == TMP.core then
                rawDeps
            else
                Module.defaultImports ++ rawDeps

      return (name, (PackageData filePath deps, addParent (Just name) deps))


checkName :: FilePath -> Module.Name -> Maybe Module.Name -> Task.Task ()
checkName path nameFromSource maybeName =
    case maybeName of
      Nothing ->
          return ()

      Just nameFromPath ->
          if nameFromSource == nameFromPath then
              return ()

          else
              Task.throw (Error.ModuleNameFileNameMismatch path nameFromPath nameFromSource)


addParent :: Maybe Module.Name -> [Module.Name] -> [(Module.Name, Maybe Module.Name)]
addParent maybeParent names =
    map (\name -> (name, maybeParent)) names


-- EXPOSED MODULES in DEPENDENCIES

getExposedModulesInDependencies
    :: Desc.Description
    -> S.Solution
    -> Task.Task (Map.Map Module.Name [(Pkg.Name, Pkg.Version)])
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
