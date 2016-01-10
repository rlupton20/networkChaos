{-# LANGUAGE RecordWildCards #-}
module Control.IO.Builder.Worker where

import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TQueue

-- |Wrap the channel type so we don't need to know
-- precisely what it is from the outside
type WorkQueue a = TQueue a

newWorkQueue :: IO (WorkQueue a)
newWorkQueue = newTQueueIO

readWorkQueue :: WorkQueue a -> IO a
readWorkQueue = atomically.readTQueue

writeWorkQueue :: WorkQueue a -> a -> IO ()
writeWorkQueue wq v = atomically $ writeTQueue wq v 

data Worker a = Worker { wq :: WorkQueue a
                       , action :: a -> IO () }

newWorker :: (a -> IO ()) -> IO (Worker a)
newWorker action = do
  wq <- newWorkQueue
  return $ Worker wq action

passWork :: a -> Worker a -> IO ()
passWork job Worker{..} = writeWorkQueue wq job
