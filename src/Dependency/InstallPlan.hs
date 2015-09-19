module Dependency.InstallPlan
    ( Plan(..)
    , for
    , isEmpty, hasChanges
    , toString
    )
    where

import Control.Monad.Except (liftIO)
import qualified Data.Map as Map

import qualified Dependency.Build as Build
import qualified Dependency.Solution as S
import qualified Dependency.Solver as Solver
import qualified Elm.Package as Pkg
import qualified Fetch.Artifacts as Artifacts
import qualified Fetch.GitHub as GitHub
import qualified Package.Description as D
import qualified Task


-- INSTALLATION PLAN

data Plan = Plan
    { installs :: Map.Map Pkg.Name Pkg.Version
    , upgrades :: Map.Map Pkg.Name (Pkg.Version, Pkg.Version)
    , removals :: Map.Map Pkg.Name Pkg.Version
    }


-- RUN THE PLAN

run :: Plan -> Task.Task ()
run (Plan installs upgrades removals) =
  let
    allInstalls =
      Map.toList installs ++ Map.toList (Map.map snd upgrades)

    allRemovals =
      Map.toList removals ++ Map.toList (Map.map fst upgrades)
  in
    do  -- fetch new dependencies
        mapM_ (uncurry GitHub.getSourceCode) allInstalls

        -- try to build new dependencies
        error "TODO"

        -- remove dependencies that are not needed
        liftIO $ mapM_ (uncurry Artifacts.removeSourceCode) allRemovals


-- CREATE PLAN

for :: D.Description -> Task.Task Plan
for description =
  do  newSolution <- Solver.solve (D.dependencies description)
      oldSolution <- Artifacts.getSolvedDependencies
      return (create oldSolution newSolution)


create :: S.Solution -> S.Solution -> Plan
create old new =
  Plan
    { installs = Map.difference new old
    , upgrades = discardNoOps (Map.intersectionWith (,) old new)
    , removals = Map.difference old new
    }
  where
    discardNoOps updates =
      Map.mapMaybe isChanged updates

    isChanged upgrade@(oldVersion,newVersion) =
      if oldVersion == newVersion
        then Nothing
        else Just upgrade


isEmpty :: Plan -> Bool
isEmpty (Plan installs upgrades removals) =
    Map.null installs
        && Map.null upgrades
        && Map.null removals


hasChanges :: Plan -> Bool
hasChanges (Plan _ upgrades removals) =
    not (Map.null upgrades)
    || not (Map.null removals)


-- DISPLAY

toString :: Plan -> String
toString (Plan installs upgrades removals) =
    "\n"
    ++ displayCategory "Install" displayInstall installs
    ++ displayCategory "Upgrade" displayUpgrade upgrades
    ++ displayCategory "Remove" displayRemove removals
  where
    displayCategory name render category =
        if Map.null category then "" else
          "  " ++ name ++ ":"
          ++ concatMap (\entry -> "\n    " ++ render entry) (Map.toList category)
          ++ "\n"

    displayInstall (name, version) =
        Pkg.toString name ++ " " ++ Pkg.versionToString version

    displayUpgrade (name, (old, new)) =
        Pkg.toString name ++ " ("
        ++ Pkg.versionToString old ++ " => " ++ Pkg.versionToString new ++ ")"

    displayRemove (name, _version) =
        Pkg.toString name