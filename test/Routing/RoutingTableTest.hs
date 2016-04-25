module Routing.RoutingTableTest
( routingTableTest ) where

import Test.HUnit

import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
       
import Routing.RoutingTable.Internal
import Types

routingTableTest = TestList $ [addAndRetrieveRoute, lookupNoRoute]

-- We need an to pass to newRoutingTable, but it
-- doesn't actually need to be touched.
duffInjector :: Injector
duffInjector = undefined

-- The same is true of our work queue
duffQueue :: TQueue Packet
duffQueue = undefined

makeTestRT :: IO RoutingTable
makeTestRT = newRoutingTable duffInjector

addAndRetrieveRoute :: Test
addAndRetrieveRoute = "getDirectionWith retrieves address entered by newRoute" ~: test
 where
   test = do
     rt <- makeTestRT
     let label = addrW8 10 0 0 1
         item = addrW8 192 168 3 1
     newRoute rt label (item, duffQueue)
     lookup <- label `getDirectionWith` rt
     Just item @=? fmap fst lookup

lookupNoRoute :: Test
lookupNoRoute = "getDirectionWith an address that hasn't been added" ~: test
  where
    test = do
      rt <- makeTestRT
      let noLabel = addrW8 10 10 10 10
      lookup <- noLabel `getDirectionWith` rt
      Nothing @=? fmap fst lookup  -- Need an instance of Show for @=?
