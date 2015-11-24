module Routing.Routing
( makeRouter
, routeTo ) where

import Routing.RoutingTable
import Routing.PacketParsing.IP4

import qualified Data.ByteString as B
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

makeRouter :: IO (TQueue B.ByteString, RoutingTable)
makeRouter = do
  buf <- newTQueueIO
  table <- newRoutingTable

  tid <- forkIO $ buf `routeWith` table
  putStrLn $ "New router on " ++ show tid

  return (buf, table)

routeTo :: a -> (TQueue a) -> IO ()
routeTo x xq = atomically $ writeTQueue xq x
  
routeWith :: (TQueue B.ByteString) -> RoutingTable -> IO ()
routeWith bsq _ = loop
  where
    loop = do
           bs <- atomically $ readTQueue bsq
           putStrLn.show $ parseIP4 bs
           loop
