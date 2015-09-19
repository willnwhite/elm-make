module Task.Prompt where

import Control.Monad.Except (liftIO)
import System.IO (hFlush, stdout)

import qualified Dependency.InstallPlan as Plan
import qualified Task


data Question
    = ApprovePlan Plan.Plan


ask :: Question -> Task.Task ()
ask question =
  case question of
    ApprovePlan plan ->
        if error "TODO autoYes" || not (Plan.hasChanges plan) then
          return ()

        else
          do  liftIO $ putStr $
                  "Some new packages are needed. Here is the upgrade plan.\n"
                  ++ Plan.toString plan
                  ++ "\nDo you approve of this plan? [Y/n] "
              yesOrNo "Okay, I did not change anything!"


yesOrNo :: String -> Task.Task ()
yesOrNo message =
  do  liftIO (hFlush stdout)
      input <- liftIO getLine
      case input of
        "" ->
            return ()

        "y" ->
            return ()

        "n" ->
            do  liftIO (putStrLn message)
                Task.quit

        _ ->
            do  liftIO (putStr "Must type 'y' for yes or 'n' for no: ")
                yesOrNo message
