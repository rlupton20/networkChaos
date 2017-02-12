module Routing.RoutingTable
( (#->)
, RoutingTable
, Injector
, newRoutingTable
, getAddr
, setAddr
, getDirectionWith
, getOutChannelFrom
, getInjector
, withRoute) where

-- This is an interface file for Routing.RoutingTable.Internal
import Routing.RoutingTable.Internal
import Core (Injector)
