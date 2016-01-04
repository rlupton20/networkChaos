module ProcUnit
( ProcUnit
, procUnit
, passTo ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Monad

-- |ProcUnit represents a thread which repeatedly takes
-- an input of type a, does some processing, and creates
-- some output of type b
data ProcUnit a b = ProcUnit { tid :: ThreadId
                             , inchan :: TQueue a }

-- |procUnit creates a ProcUnit from an IO operation
procUnit :: (a -> IO b) -> IO (ProcUnit a b)
procUnit action = do
  inchan <- newTQueueIO
  tid <- forkIO $ forever $ do
    bp <- atomically $ readTQueue inchan
    action bp
  return $ (ProcUnit tid inchan)

-- |passTo passes a value of type a to a ProcUnit
passTo :: a -> (ProcUnit a b) -> IO ()
passTo x (ProcUnit _ inchan) = atomically $ writeTQueue inchan x
