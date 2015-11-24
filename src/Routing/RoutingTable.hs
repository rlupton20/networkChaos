{-# LANGUAGE RecordWildCards #-}
module Routing.RoutingTable
( RoutingTable
, newRoutingTable
, setLocal
, setVirtual
, newRoute
, delRouteFor
, getDirectionWith ) where

import Routing.PacketParsing.IP4

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue

import Net.IPv4 (Addr)

import qualified Data.Map as M
import qualified Data.ByteString as B

data RoutingTable = RT { local :: (TVar Addr)
                       , virtual :: (TVar Addr)
                       , table :: TVar (M.Map Addr (Addr, TQueue B.ByteString))}

newRoutingTable :: IO RoutingTable
newRoutingTable = do
  l <- newTVarIO $ addr "0.0.0.0"
  v <- newTVarIO $ addr "0.0.0.0"
  tab <- newTVarIO $ M.empty
  let rT = (RT l v tab)
  return rT
  
setLocal :: RoutingTable -> Addr -> IO ()
setLocal RT{..} ad = atomically $ writeTVar local ad

setVirtual :: RoutingTable -> Addr -> IO ()
setVirtual RT{..} ad = atomically $ writeTVar virtual ad

newRoute :: RoutingTable -> Addr -> (Addr, TQueue B.ByteString) -> IO ()
newRoute RT{..} ad ent = atomically $ modifyTVar' table (M.insert ad ent)

delRouteFor :: RoutingTable -> Addr -> IO ()
delRouteFor RT{..} ad = atomically $ modifyTVar' table (M.delete ad)
  
getDirectionWithSTM :: Addr -> RoutingTable -> STM (Addr, Maybe (Addr, TQueue B.ByteString))
getDirectionWithSTM dest RT{..} = do
  newsrc <- readTVar virtual
  rt <- readTVar table
  let newdest = dest `M.lookup` rt 
  return (newsrc, newdest)

getDirectionWith :: Addr -> RoutingTable -> IO (Addr, Maybe (Addr, TQueue B.ByteString))
getDirectionWith ad rt = atomically $ getDirectionWithSTM ad rt
