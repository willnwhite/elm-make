{-# LANGUAGE FlexibleContexts #-}
module CLI.Helpers where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Trans (MonadIO, liftIO)
import System.Directory
import System.IO

import qualified Elm.Utils as Utils


inDir :: (MonadError String m, MonadIO m) => FilePath -> m a -> m a
inDir dir doStuff =
  do  here <- liftIO $ getCurrentDirectory
      liftIO $ createDirectoryIfMissing True dir
      liftIO $ setCurrentDirectory dir
      result <- doStuff
      liftIO $ setCurrentDirectory here
      return result


git :: (MonadError String m, MonadIO m) => [String] -> m String
git = run "git"


run :: (MonadError String m, MonadIO m) => String -> [String] -> m String
run = Utils.run


out :: (MonadIO m) => String -> m ()
out string =
    liftIO $ hPutStrLn stdout string'
  where
    string' =
        if not (null string) && last string == '\n' then init string else string

