module Routing.RoutingTable
( (#->)
, RoutingTable
, Injector
, newRoutingTable
, setAddr
, getDirectionWith
, getOutChannelFrom
, getInjector
, withRoute) where

-- This is an interface file for Routing.RoutingTable.Internal
import Routing.RoutingTable.Internal
import Types (Injector)
