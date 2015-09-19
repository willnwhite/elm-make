{-# OPTIONS_GHC -Wall #-}
module CLI.Make (run) where

import qualified Task


run :: Int -> [String] -> Task.Task ()
run numProcessors args =
  error "TODO"

{-
      (Crawl.ProjectInfo thisPackage exposedModules moduleForGeneration projectSummary) <-
          M.phase "Crawl Project" (Crawl.crawl config)

      let dependencies =
            Map.map projectDependencies (projectData projectSummary)

      let modulesToDocument =
            maybe Set.empty (const exposedModules) (M._docs config)

      buildSummary <-
          M.phase "Plan Build" (Plan.planBuild config modulesToDocument projectSummary)

      docs <-
        M.phase "Compile" $ liftIO $
          Compile.build
            config
            numProcessors
            thisPackage
            exposedModules
            moduleForGeneration
            dependencies
            buildSummary

      M.phase "Generate Docs" $
        maybe (return ()) (Generate.docs docs) (M._docs config)

      M.phase "Generate Code" $
        Generate.generate
          config
          dependencies
          (projectNatives projectSummary)
          moduleForGeneration
-}
