{-# LANGUAGE BangPatterns #-}
module Routing.Routing
( makeRouter ) where

import qualified Data.ByteString as B

import Network.Vanguard.Core
import Routing.RoutingTable

-- |Takes an Injector (PacketQueue), and creates a new
-- routing table, returning this routing table along with
-- a description of an IO process for routing packets.
makeRouter :: Injector -> IO (Packet -> IO (), RoutingTable)
makeRouter inj = do
  table <- newRoutingTable inj
  let route x = x `routeWith` table
  return (route, table)

-- |routeWith takes a packet in the form of a ByteString, and
-- sends it to the approprtiate thread for forwarding, using
-- the RoutingTable for lookup.
routeWith :: Packet -> RoutingTable -> IO ()
routeWith bs rt = do
  dest <- lookup bs
  case dest of
    Just outchan -> bs `passTo` outchan
    Nothing -> return ()
  where
    lookup :: Packet -> IO (Maybe PacketQueue)
    lookup bs = do
      let dest = getDest bs
      dest `getOutChannelFrom` rt

    getDest :: Packet -> Addr
    getDest bs = let a = bs `B.index` 16
                     b = bs `B.index` 17
                     c = bs `B.index` 18
                     d = bs `B.index` 19 in
                 addrW8 a b c d
