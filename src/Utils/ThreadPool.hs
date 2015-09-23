module Utils.ThreadPool
    ( ThreadPool
    , threadPool
    , walkGraph
    )
    where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Monad.Trans (liftIO)

import qualified Task
import qualified Utils.Graph as Graph



-- WALK A GRAPH


walkGraph :: (Ord key) => ((key,value) -> IO result) -> Graph.Graph key value -> Task.Task [result]
walkGraph handler graph =
  let
    roots =
      Graph.roots graph
  in
    do  pool <- liftIO (threadPool handler)
        liftIO (mapM_ (send pool) roots)
        walker pool (length roots) graph []


walker :: (Ord k) => ThreadPool (k,v) r -> Int -> Graph.Graph k v -> [r] -> Task.Task [r]
walker pool activeThreads graph results =
  do  -- wait for some work to finish
      ((key, _), result) <- liftIO (recv pool)

      -- collect results
      let newResults = result : results

      -- spawn workers for new roots
      let (newGraph, newRoots) = Graph.removeRoot key graph
      liftIO (mapM_ (send pool) newRoots)
      let newActiveThreads = activeThreads - 1 + length newRoots

      -- decide how to proceed
      if newActiveThreads == 0
        then
          return newResults

        else
          walker pool newActiveThreads newGraph newResults



-- THREAD POOLS


data ThreadPool a b =
  ThreadPool
    { send :: a -> IO ()
    , recv :: IO (a, b)
    , kill :: IO ()
    }


data Message a = Kill | Msg a


threadPool :: (a -> IO b) -> IO (ThreadPool a b)
threadPool handler =
  do  requestChan <- newChan
      responseChan <- newChan

      let worker a =
            do  b <- handler a
                writeChan responseChan (a, b)

      let supervisor =
            do  msg <- readChan requestChan
                case msg of
                  Kill ->
                      return ()

                  Msg a ->
                      do  forkIO (worker a)
                          supervisor

      forkIO supervisor

      return $ ThreadPool
        { send = (\a -> writeChan requestChan (Msg a))
        , recv = readChan responseChan
        , kill = writeChan requestChan Kill
        }




