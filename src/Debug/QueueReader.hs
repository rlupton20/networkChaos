module Debug.QueueReader
( makeQueueReader
, newQueueAndReader ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

import qualified Data.ByteString as B

makeQueueReader :: TQueue a -> (a -> IO b) -> IO ThreadId
makeQueueReader tq action = do
  tid <- forkIO $ loop
  putStrLn $ "Queue reader on " ++ show tid
  return tid
    where loop = do
            v <- atomically $ readTQueue tq
            action v
            loop

newQueueAndReader :: (B.ByteString -> IO a) -> IO (TQueue B.ByteString)
newQueueAndReader action = do
  q <- newTQueueIO
  tid <- forkIO $ (loop q)
  putStrLn $ "QueueReader: " ++ show tid
  return q
  where
    loop q = do
      bs <- atomically $ readTQueue q
      action bs
      loop q
      
