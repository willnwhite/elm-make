module Dependency.Solver (solve) where

import qualified Control.Monad.State as State
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Dependency.Constraint as C
import qualified Dependency.Solution as S
import qualified Elm.Compiler as Compiler
import qualified Elm.Package as Pkg
import qualified Fetch.PackageWebsite as Website
import qualified Package.Description as Desc
import qualified Task
import qualified TheMasterPlan as TMP
import qualified Utils.Graph as Graph


-- ACTUALLY SOLVE THE CONSTRAINTS

solve :: [(Pkg.Name, C.Constraint)] -> Task.Task (S.Solution, Graph.Graph Pkg.Name Pkg.Version)
solve constraints =
  do  versions <- Website.allPackages

      (maybeSolution, MetaData allConstraints _) <-
          State.runStateT (exploreConstraints constraints) (MetaData Map.empty versions)

      case maybeSolution of
        Nothing ->
            Task.throw $ error "TODO" $
              "Unable to find a set of packages that will work with your constraints."

        Just solution ->
            let
              neededPackages =
                Map.toList solution

              toNode pkg@(name, version) =
                ( name
                , version
                , map fst (snd ((Map.!) allConstraints pkg))
                )
            in
              return (solution, Graph.fromList (map toNode neededPackages))



-- EXPLORER
--
-- To avoid HTTP requests and disk reads, we cache all available versions of
-- packages and the constraints of all the packages we have looked at so far.


type Explorer =
    State.StateT MetaData Task.Task


data MetaData = MetaData
    { _constraints :: Map.Map TMP.Package PkgConstraints
    , _versions :: VersionsDict
    }


type VersionsDict =
    Map.Map Pkg.Name [Pkg.Version]


type PkgConstraints =
    (C.Constraint, [(Pkg.Name, C.Constraint)])


getConstraints :: Pkg.Name -> Pkg.Version -> Explorer PkgConstraints
getConstraints name version =
  do  (MetaData constraintsDict versionsDict) <- State.get

      case Map.lookup (name, version) constraintsDict of
        Just constraints ->
            return constraints

        Nothing ->
            do  desc <- State.lift (Website.description name version)

                let constraints =
                      (Desc.elmVersion desc, Desc.dependencies desc)

                let newConstraintDict =
                      Map.insert (name, version) constraints constraintsDict

                State.put (MetaData newConstraintDict versionsDict)

                return constraints


getVersions :: Pkg.Name -> Explorer [Pkg.Version]
getVersions name =
  do  versions <- State.gets _versions
      case Map.lookup name versions of
        Just versions ->
            return versions

        Nothing ->
            State.lift (Task.throw (error "TODO"))



-- EXPLORE CONSTRAINTS


exploreConstraints :: [(Pkg.Name, C.Constraint)] -> Explorer (Maybe S.Solution)
exploreConstraints constraints =
  do  maybeInitialPackages <- addConstraints Map.empty constraints
      let initialPackages = maybe Map.empty id maybeInitialPackages
      explorePackages Map.empty initialPackages


explorePackages :: S.Solution -> VersionsDict -> Explorer (Maybe S.Solution)
explorePackages solution availablePackages =
    case Map.minViewWithKey availablePackages of
      Nothing ->
          return (Just solution)

      Just ((name, versions), remainingPackages) ->
          exploreVersionList name versions solution remainingPackages


exploreVersionList :: Pkg.Name -> [Pkg.Version] -> S.Solution -> VersionsDict -> Explorer (Maybe S.Solution)
exploreVersionList name versions solution remainingPackages =
    go (reverse (Pkg.filterLatest Pkg.majorAndMinor versions))
  where
    go versions =
        case versions of
          [] -> return Nothing
          version : rest ->
              do  maybeSolution <- exploreVersion name version solution remainingPackages
                  case maybeSolution of
                    Nothing -> go rest
                    answer -> return answer


exploreVersion :: Pkg.Name -> Pkg.Version -> S.Solution -> VersionsDict -> Explorer (Maybe S.Solution)
exploreVersion name version solution remainingPackages =
  do  (elmVersion, constraints) <- getConstraints name version
      if C.isSatisfied elmVersion Compiler.version
        then explore constraints
        else return Nothing

  where
    explore constraints =
      let
        (overlappingConstraints, newConstraints) =
          List.partition (\(name, _) -> Map.member name solution) constraints
      in
        case all (satisfiedBy solution) overlappingConstraints of
          False ->
              return Nothing

          True ->
              do  maybePackages <- addConstraints remainingPackages newConstraints
                  case maybePackages of
                    Nothing ->
                        return Nothing

                    Just extendedPackages ->
                        explorePackages (Map.insert name version solution) extendedPackages


satisfiedBy :: S.Solution -> (Pkg.Name, C.Constraint) -> Bool
satisfiedBy solution (name, constraint) =
    case Map.lookup name solution of
      Nothing ->
          False

      Just version ->
          C.isSatisfied constraint version


addConstraints :: VersionsDict -> [(Pkg.Name, C.Constraint)] -> Explorer (Maybe VersionsDict)
addConstraints packages constraints =
    case constraints of
      [] ->
          return (Just packages)

      (name, constraint) : rest ->
          do  versions <- getVersions name
              case filter (C.isSatisfied constraint) versions of
                [] -> return Nothing
                vs -> addConstraints (Map.insert name vs packages) rest
