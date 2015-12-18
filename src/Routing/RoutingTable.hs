{-# LANGUAGE RecordWildCards #-}
module Routing.RoutingTable
( RoutingTable
, newRoutingTable
, setAddr
, newRoute
, delRouteFor
, getDirectionWith
, getInjectionQueue ) where

import Routing.PacketParsing.IP4

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue

import Net.IPv4 (Addr)

import qualified Data.Map as M
import qualified Data.ByteString as B

data RoutingTable = RT { ipadd :: (TVar Addr)
                       , inject :: TVar (TQueue B.ByteString)
                       , table :: TVar (M.Map Addr (Addr, TQueue B.ByteString))}

newRoutingTable :: IO RoutingTable
newRoutingTable = do
  ipadd <- newTVarIO $ addr "0.0.0.0"
  injq <- newTQueueIO
  inj <- newTVarIO injq
  tab <- newTVarIO $ M.empty
  let rT = (RT ipadd inj tab)
  return rT
  
setAddr :: RoutingTable -> Addr -> IO ()
setAddr RT{..} ad = atomically $ writeTVar ipadd ad

newRoute :: RoutingTable -> Addr -> (Addr, TQueue B.ByteString) -> IO ()
newRoute RT{..} ad ent = atomically $ modifyTVar' table (M.insert ad ent)

delRouteFor :: RoutingTable -> Addr -> IO ()
delRouteFor RT{..} ad = atomically $ modifyTVar' table (M.delete ad)
  
getDirectionWithSTM :: Addr -> RoutingTable -> STM (Maybe (Addr, TQueue B.ByteString))
getDirectionWithSTM dest RT{..} = do
  rt <- readTVar table
  let newdest = dest `M.lookup` rt 
  return newdest

getDirectionWith :: Addr -> RoutingTable -> IO (Maybe (Addr, TQueue B.ByteString))
getDirectionWith ad rt = atomically $ getDirectionWithSTM ad rt

getInjectionQueue :: RoutingTable -> IO (TQueue B.ByteString)
getInjectionQueue RT{..} = atomically $ readTVar inject
