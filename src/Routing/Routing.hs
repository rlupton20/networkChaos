{-# LANGUAGE BangPatterns #-}
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
                 Just (newsrc, rchan) -> do
                   let nbs =  toBytes $ ppck `setSource` newsrc
                   atomically $ writeTQueue rchan nbs
                 Nothing -> return ()
             Nothing -> return ()
           loop

    lookup ppck = do
      let dest = getDest ppck
      (newsrc, redirect) <- dest `getDirectionWith` rt
      return $ redirect >>= (\(_, routechan) -> Just (newsrc, routechan) )
