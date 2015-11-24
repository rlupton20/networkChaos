module Debug.QueueReader
( makeQueueReader ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

import qualified Data.ByteString as B

makeQueueReader :: (B.ByteString -> IO a) -> IO (TQueue B.ByteString)
makeQueueReader action = do
  q <- newTQueueIO
  tid <- forkIO $ (loop q)
  putStrLn $ "QueueReader: " ++ show tid
  return q
  where
    loop q = do
      bs <- atomically $ readTQueue q
      action bs
      loop q
      
