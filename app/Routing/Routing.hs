{-# LANGUAGE BangPatterns #-}
module Routing.Routing
( makeRouter ) where

import Types
  
import Routing.RoutingTable

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue

import qualified Data.ByteString as B

-- |Takes an injection ProcUnit (Injector), and launches a new
-- routing ProcUnit, after creating a new routing table which it
-- uses. The routing table is also returned.
makeRouter :: PacketQueue -> IO (Packet -> IO (), RoutingTable)
makeRouter inj = do
  table <- newRoutingTable inj
  let route x = x `routeWith` table
  return (route, table)

-- |routeWith takes a packet in the form of a ByteString, and
-- sends it to the approprtiate thread for forwarding, using
-- the RoutingTable for lookup.
routeWith :: Packet -> RoutingTable -> IO ()
routeWith bs rt = do
  redir <- lookup bs
  case redir of
    Just rchan -> atomically $ writeTQueue rchan bs
    Nothing -> return ()
  where
    lookup :: Packet -> IO (Maybe (TQueue B.ByteString))
    lookup bs = do
      let dest = getDest bs
      redirect <- dest `getDirectionWith` rt
      return $ fmap snd redirect

    getDest :: Packet -> Addr
    getDest bs = let a = bs `B.index` 16
                     b = bs `B.index` 17
                     c = bs `B.index` 18
                     d = bs `B.index` 19 in
                 addrW8 a b c d
