module Dependency.Explorer
    ( Explorer
    , run
    , lift
    , getConstraints
    , getVersions
    )
    where

import qualified Control.Monad.State as State
import qualified Data.Map as Map

import qualified Dependency.Constraint as C
import qualified Elm.Package as Pkg
import qualified Fetch.PackageWebsite as Website
import qualified Package.Description as Desc
import qualified Task


-- TASK

type Explorer =
    State.StateT MetaData Task.Task


run :: Explorer a -> Task.Task a
run solverTask =
  do  versions <- Website.allPackages
      State.evalStateT solverTask (MetaData Map.empty versions)


lift :: Task.Task a -> Explorer a
lift =
  State.lift


-- METADATA

data MetaData = MetaData
    { _constraints :: Constraints
    , _versions :: Versions
    }


type Constraints =
    Map.Map (Pkg.Name, Pkg.Version) (C.Constraint, [(Pkg.Name, C.Constraint)])


type Versions =
    Map.Map Pkg.Name [Pkg.Version]


-- CONSTRAINTS

getConstraints :: Pkg.Name -> Pkg.Version -> Explorer (C.Constraint, [(Pkg.Name, C.Constraint)])
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


-- VERSIONS

getVersions :: Pkg.Name -> Explorer [Pkg.Version]
getVersions name =
  do  versions <- State.gets _versions
      case Map.lookup name versions of
        Just versions ->
            return versions

        Nothing ->
            State.lift (Task.throw (error "TODO"))
