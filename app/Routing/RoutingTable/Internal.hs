{-# LANGUAGE RecordWildCards #-}
module Routing.RoutingTable.Internal where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception (bracket_)

import qualified Data.Map as M

import Core

-- |Route is a datatype describing a routing entry.
type Route = (Addr, (Addr, PacketQueue))

-- | Given a Route, local extracts the address as we would see
-- it on a local system.
local :: Route -> Addr
local = fst

-- |outgoing takes a Route and returns the routing table entry
-- for tha route, i.e. the external address it is going to, and
-- the queue to place a packet on.
outgoing :: Route -> (Addr, PacketQueue)
outgoing = snd

-- |external determines the external address for a route.
external :: Route -> Addr
external = fst . snd

-- |#-> is a combinator for building Routes. It makes it easy to
-- see how a packet would be routed by a route. For instance
-- addr #-> (out, queue) describes the routing of packets destined
-- for addr being routed to out, and immediately being placed on queue.
(#->) :: Addr -> (Addr, PacketQueue) -> Route
local #-> outgoing = (local, outgoing)


data RoutingTable = RT { ip :: (TVar Addr)
                       , injector :: Injector
                       , table :: TVar (M.Map Addr (Addr, PacketQueue))}


-- |newRoutingTable takes an Injector (a blue print for a thread
-- which can be used to put packets back into the system), and
-- creates an empty RoutingTable which uses that Injector.
newRoutingTable :: Injector -> IO RoutingTable
newRoutingTable injector = do
  ip <- newTVarIO $ addrW8 0 0 0 0
  table <- newTVarIO $ M.empty
  let rt = (RT ip injector table)
  return rt

-- |setAddr records the address of this instance to the RoutingTable.
setAddr :: RoutingTable -> Addr -> IO ()
setAddr RT{..} ad = atomically $ writeTVar ip ad

-- |getAddr gets the IP address we are using on the network
getAddr :: RoutingTable -> IO Addr
getAddr RT{..} = atomically $ readTVar ip

-- |newRoute adds a new route to the routing table. An endpoint is described
-- by a 2-tuple consisting of an address, and a Queue of Packets.
newRoute :: RoutingTable -> Addr -> (Addr, PacketQueue) -> IO ()
newRoute RT{..} ad entry = atomically $ modifyTVar' table (M.insert ad entry)

-- |delRouteFor deletes a route associated with an address.
delRouteFor :: RoutingTable -> Addr -> IO ()
delRouteFor RT{..} ad = atomically $ modifyTVar' table (M.delete ad)

-- |getDirectionWithSTM is the STM version of looking up an endpoint in a
-- Routing Table.
getDirectionWithSTM :: Addr -> RoutingTable -> STM (Maybe (Addr, PacketQueue))
getDirectionWithSTM dest RT{..} = do
  rt <- readTVar table
  let newdest = dest `M.lookup` rt 
  return newdest

-- |getDirectionWith is the atomic (IO) version of getDirectionWithSTM.
getDirectionWith :: Addr -> RoutingTable -> IO (Maybe (Addr, PacketQueue))
getDirectionWith ad rt = atomically $ getDirectionWithSTM ad rt

-- |getOutChannel just returns the channel obtained with
-- getDirectionWith.
getOutChannelFrom :: Addr -> RoutingTable -> IO (Maybe PacketQueue)
getOutChannelFrom ad rt = fmap (fmap snd) $ ad `getDirectionWith` rt

-- |getInjector returns the Injector associated with a RoutingTable
getInjector :: RoutingTable -> Injector
getInjector RT{..} = injector

-- |withRoute is allows an IO action to be executed but with a route
-- added to the routing table. Once that IO action is completed, the
-- route is removed from the routing table. In other words withRoute
-- is a bracketed way of adding a route to a table, which ensures the
-- route is removed in the case of exceptions etc. 
withRoute :: RoutingTable -> Route -> IO () -> IO ()
withRoute rt route action = bracket_ (newRoute rt (local route) (outgoing route))
                                     (rt `delRouteFor` (local route))
                                     action
