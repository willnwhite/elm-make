module Package.Install where

import Control.Applicative ((<$>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans (liftIO)
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import System.Directory (doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>))

import qualified Dependency.Constraint as Constraint
import qualified Dependency.Solution as S
import qualified Elm.Package as Pkg
import qualified Fetch.Artifacts as Artifacts
import qualified Fetch.GitHub as GitHub
import qualified Fetch.PackageWebsite as Website
import qualified Package.Description as D
import qualified Task
import qualified Task.Prompt as Prompt


-- CREATE elm-package.json

createDescription :: Task.Task D.Description
createDescription =
  do  latestVersion <-
          maximum <$> Website.versions Pkg.coreName

      let coreConstraint =
            (Pkg.coreName, Constraint.untilNextMajor latestVersion)

      let description =
            D.defaultDescription { D.dependencies = [coreConstraint] }

      liftIO (D.write description)

      return description


{-- INSTALL EVERYTHING

dependencies =
  do  plan <- Plan.for description
      Prompt.ask (Prompt.ApprovePlan plan)
      runPlan newSolution plan


runPlan :: S.Solution -> Plan.Plan -> Task.Task ()
runPlan solution (Plan.Plan installs upgrades removals) =
  let
    allInstalls =
      Map.toList installs ++ Map.toList (Map.map snd upgrades)

    allRemovals =
      Map.toList removals ++ Map.toList (Map.map fst upgrades)
  in
    do  -- fetch new dependencies
        mapM_ (uncurry GitHub.getSourceCode) allInstalls

        -- try to build new dependencies
        liftIO (S.write Path.solvedDependencies solution)

        -- remove dependencies that are not needed
        liftIO $ mapM_ (uncurry Artifacts.removeSourceCode) removals


data Args
    = Everything
    | Latest Pkg.Name
    | Exactly Pkg.Name Pkg.Version


install :: Bool -> Args -> Task.Task ()
install autoYes args =
  do  exists <- liftIO (doesFileExist Path.description)

      description <-
          case exists of
            True -> Desc.read Path.description
            False -> initialDescription

      case args of
        Everything ->
            upgrade autoYes description

        Latest name ->
            do  version <- maximum <$> Website.versions name
                newDescription <- addConstraint autoYes name version description
                upgrade autoYes newDescription

        Exactly name version ->
            do  newDescription <- addConstraint autoYes name version description
                upgrade autoYes newDescription


-- MODIFY DESCRIPTION

addConstraint :: Bool -> Pkg.Name -> Pkg.Version -> Desc.Description -> Task.Task Desc.Description
addConstraint autoYes name version description =
  case List.lookup name (Desc.dependencies description) of
    Nothing ->
      addNewDependency autoYes name version description

    Just constraint
      | Constraint.isSatisfied constraint version ->
          return description

      | otherwise ->
          throwError $
            "This is a tricky update, you should modify " ++ Path.description ++ " yourself.\n"
            ++ "Package " ++ Pkg.toString name ++ " is already listed as a dependency:\n\n    "
            ++ showDependency name constraint ++ "\n\n"
            ++ "You probably want one of the following constraints instead:\n\n    "
            ++ Constraint.toString (Constraint.expand constraint version) ++ "\n    "
            ++ Constraint.toString (Constraint.untilNextMajor version) ++ "\n"


addNewDependency :: Bool -> Pkg.Name -> Pkg.Version -> Desc.Description -> Task.Task Desc.Description
addNewDependency autoYes name version description =
  do  confirm <-
          case autoYes of
            True -> return True
            False ->
              do  answer <- liftIO confirmNewAddition
                  liftIO (putStrLn "")
                  return answer

      case confirm of
        False ->
          do  liftIO $ putStrLn noConfirmation
              return description
        True ->
          do  let newDescription = description { Desc.dependencies = newConstraints }
              liftIO $ Desc.write newDescription
              return newDescription
  where
    newConstraint =
        Constraint.untilNextMajor version

    newConstraints =
        (name, newConstraint) : Desc.dependencies description

    noConfirmation =
        "Cannot install the new package unless it appears in " ++ Path.description ++ ".\n" ++
        "If you do not like the constraint I suggested, change it manually and then run:\n" ++
        "\n    elm-package install\n\n" ++
        "This will install everything listed in " ++ Path.description ++ "."

    confirmNewAddition =
      do  putStrLn $
            "To install " ++ Pkg.toString name ++ " I would like to add the following\n"
            ++ "dependency to " ++ Path.description ++ ":\n\n    "
            ++ showDependency name newConstraint
            ++ "\n"

          putStr $ "May I add that to " ++ Path.description ++ " for you? (y/n) "
          Cmd.yesOrNo


showDependency :: Pkg.Name -> Constraint.Constraint -> String
showDependency name constraint =
    show (Pkg.toString name) ++ ": " ++ show (Constraint.toString constraint)


--}
