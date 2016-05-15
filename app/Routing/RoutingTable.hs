module Routing.RoutingTable
( RoutingTable
, Injector
, newRoutingTable
, setAddr
, newRoute
, delRouteFor
, getDirectionWith
, getOutChannelFrom
, getInjector ) where

-- This is an interface file for Routing.RoutingTable.Internal
import Routing.RoutingTable.Internal
import Types (Injector)
