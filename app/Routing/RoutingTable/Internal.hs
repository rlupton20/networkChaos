{-# LANGUAGE RecordWildCards #-}
module Routing.RoutingTable.Internal where

import Types

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue

import qualified Data.Map as M

data RoutingTable = RT { ipadd :: (TVar Addr)
                       , inject :: PacketQueue
                       , table :: TVar (M.Map Addr (Addr, TQueue Packet))}


-- |newRoutingTable takes an Injector (a blue print for a thread
-- which can be used to put packets back into the system), and
-- creates an empty RoutingTable which uses that Injector.
newRoutingTable :: PacketQueue -> IO RoutingTable
newRoutingTable inj = do
  ipadd <- newTVarIO $ addrW8 0 0 0 0
  tab <- newTVarIO $ M.empty
  let rT = (RT ipadd inj tab)
  return rT

-- |setAddr records the address of this instance to the RoutingTable.
setAddr :: RoutingTable -> Addr -> IO ()
setAddr RT{..} ad = atomically $ writeTVar ipadd ad

-- |newRoute adds a new route to the routing table. An endpoint is described
-- by a 2-tuple consisting of an address, and a TQueue of Packets.
newRoute :: RoutingTable -> Addr -> (Addr, TQueue Packet) -> IO ()
newRoute RT{..} ad ent = atomically $ modifyTVar' table (M.insert ad ent)

-- |delRouteFor deletes a route associated with an address.
delRouteFor :: RoutingTable -> Addr -> IO ()
delRouteFor RT{..} ad = atomically $ modifyTVar' table (M.delete ad)

-- |getDirectionWithSTM is the STM version of looking up an endpoint in a
-- Routing Table.
getDirectionWithSTM :: Addr -> RoutingTable -> STM (Maybe (Addr, TQueue Packet))
getDirectionWithSTM dest RT{..} = do
  rt <- readTVar table
  let newdest = dest `M.lookup` rt 
  return newdest

-- |getDirectionWith is the atomic (IO) version of getDirectionWithSTM.
getDirectionWith :: Addr -> RoutingTable -> IO (Maybe (Addr, TQueue Packet))
getDirectionWith ad rt = atomically $ getDirectionWithSTM ad rt

-- |getInjector returns the Injector associated with a RoutingTable
getInjector :: RoutingTable -> PacketQueue
getInjector RT{..} = inject

