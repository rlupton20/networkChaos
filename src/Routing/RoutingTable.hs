module Routing.RoutingTable
( RoutingTable
, newRoutingTable ) where

import Control.Concurrent.STM.TVar

type RoutingTable = TVar Int

newRoutingTable :: IO RoutingTable
newRoutingTable = do
  table <- newTVarIO 0
  return table
