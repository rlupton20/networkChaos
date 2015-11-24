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
routeWith bsq rt = loop
  where
    loop = do
           bs <- atomically $ readTQueue bsq
           let mppck = parseIP4 bs
           case mppck of
             Just ppck -> do
               redir <- lookup ppck
               case redir of
                 Just ((newsrc, newdest), rchan) -> do
                   let nbs =  ppck `readdressWith` (newsrc, newdest)
                   atomically $ writeTQueue rchan nbs
                   -- Check this works
                   out <- atomically $ readTQueue rchan
                   putStrLn $ show out
                 Nothing -> return ()
             Nothing -> return ()
           loop

    lookup ppck = do
      let dest = getDest ppck
      (newsrc, redirect) <- dest `getDirectionWith` rt
      case redirect of
        (Just (newdest, routechan)) -> return $ Just ((newsrc, newdest), routechan)
        Nothing -> return Nothing

    readdressWith ppck (newsrc, newdest) = toBytes . (`setSource` newsrc) . (`setDest` newdest) $ ppck
