module Routing
( makeRouter
, routeTo ) where

import qualified Data.ByteString as B
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

makeRouter :: IO (TQueue B.ByteString)
makeRouter = do
  buf <- newTQueueIO
  tid <- forkIO $ routeOn buf
  putStrLn $ "New router on thread " ++ show tid
  return buf

routeTo :: B.ByteString -> (TQueue B.ByteString) -> IO ()
routeTo bs bsq = atomically $ writeTQueue bsq bs
  
routeOn :: (TQueue B.ByteString) -> IO ()
routeOn bsq = loop
  where
    loop = do
           bs <- atomically $ readTQueue bsq
           putStrLn $ show bs
           loop
