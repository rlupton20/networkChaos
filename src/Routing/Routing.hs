module Routing.Routing
( makeRouter
, routeTo ) where

import Routing.RoutingTable

import qualified Data.ByteString as B
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

makeRouter :: IO (TQueue B.ByteString, RoutingTable)
makeRouter = do
  buf <- newTQueueIO
  table <- newRoutingTable

  tid <- forkIO $ routeOn buf table
  putStrLn $ "New router on " ++ show tid

  return (buf, table)

routeTo :: B.ByteString -> (TQueue B.ByteString) -> IO ()
routeTo bs bsq = atomically $ writeTQueue bsq bs
  
routeOn :: (TQueue B.ByteString) -> RoutingTable -> IO ()
routeOn bsq _ = loop
  where
    loop = do
           bs <- atomically $ readTQueue bsq
           putStrLn $ show bs
           loop
