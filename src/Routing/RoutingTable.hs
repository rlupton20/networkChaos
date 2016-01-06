{-# LANGUAGE RecordWildCards #-}
module Routing.RoutingTable
( RoutingTable
, Injector
, newRoutingTable
, setAddr
, newRoute
, delRouteFor
, getDirectionWith
, getInjector ) where

import Types

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue

import qualified Data.Map as M
import qualified Data.ByteString as B

data RoutingTable = RT { ipadd :: (TVar Addr)
                       , inject :: Injector
                       , table :: TVar (M.Map Addr (Addr, TQueue B.ByteString))}

newRoutingTable :: Injector -> IO RoutingTable
newRoutingTable inj = do
  ipadd <- newTVarIO $ addrW8 0 0 0 0
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

getInjector :: RoutingTable -> Injector
getInjector RT{..} = inject
