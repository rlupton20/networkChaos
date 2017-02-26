module Routing.RoutingTableTest
( routingTableTest ) where

import Test.Framework as TF
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit as HU
import Test.HUnit ((@=?), (~:))

import Routing.RoutingTable.Internal
import Network.Vanguard.Core

routingTableTest :: TF.Test
routingTableTest = TF.testGroup "RoutingTable.hs unit tests:" $ hUnitTestToTests routingTableUnitTests

routingTableUnitTests :: HU.Test
routingTableUnitTests = HU.TestList $ [addAndRetrieveRoute, lookupNoRoute, delRouteNoRoute, newRouteOverwrites, withRouteBracketsRoute]

-- First lets build some utility objects and functions
-- for testing with.

-- We need an to pass to newRoutingTable, but it
-- doesn't actually need to be touched.
duffInjector :: Injector
duffInjector = undefined

-- The same is true of our work queue
duffQueue :: PacketQueue
duffQueue = undefined

makeTestRT :: IO RoutingTable
makeTestRT = newRoutingTable duffInjector

-- Now lets specify the tests

addAndRetrieveRoute :: HU.Test
addAndRetrieveRoute = "newRoute:getDirectionWith: retrieves address entered by newRoute" ~: test
 where
   test = do
     rt <- makeTestRT
     let label = addrW8 10 0 0 1
         item = addrW8 192 168 3 1
     newRoute rt label (item, duffQueue)
     lookup <- label `getDirectionWith` rt
     Just item @=? fmap fst lookup

lookupNoRoute :: HU.Test
lookupNoRoute = "getDirectionWith: check it returns Nothing for an address that hasn't been added" ~: test
  where
    test = do
      rt <- makeTestRT
      let noLabel = addrW8 10 10 10 10
      lookup <- noLabel `getDirectionWith` rt
      Nothing @=? fmap fst lookup  -- Need an instance of Show for @=?

delRouteNoRoute :: HU.Test
delRouteNoRoute = "newRoute:delRouteFor: Check delRoute removes a route" ~: test
  where
    test = do
      rt <- makeTestRT

      -- First add a route and check its there
      let label = addrW8 10 0 0 1
          item = addrW8 192 168 3 1
      newRoute rt label (item, duffQueue)
      lookup <- label `getDirectionWith` rt
      Just item @=? fmap fst lookup

      -- Then delete the route and check its not
      rt `delRouteFor` label
      lookup <- label `getDirectionWith` rt
      Nothing @=? fmap fst lookup

newRouteOverwrites :: HU.Test
newRouteOverwrites = "newRoute: check newRoute overwrites an entry in a RoutingTable if an entry is already present" ~: test
  where
    test = do
      rt <- makeTestRT

      -- First add a route and check its there
      let label = addrW8 10 0 0 1
          item = addrW8 192 168 3 1
      newRoute rt label (item, duffQueue)
      lookup <- label `getDirectionWith` rt
      Just item @=? fmap fst lookup

      -- Then add a new location with the same
      -- label, and check it overwrites the old
      -- entry.

      let newItem = addrW8 101 101 101 101
      newRoute rt label (newItem, duffQueue)
      lookup <- label `getDirectionWith` rt
      Just newItem @=? fmap fst lookup


withRouteBracketsRoute :: HU.Test
withRouteBracketsRoute = "withRoute: creates a route and destroys it" ~: test
  where
    test = let label = addrW8 10 0 0 1
               external = addrW8 192 168 3 1
               outgoing = (external, duffQueue) in
             do
               rt <- makeTestRT
               rt `withRoute` (label #-> outgoing) $ do
                 lookup <- label `getDirectionWith` rt
                 Just external @=? fmap fst lookup
               lookup <- label `getDirectionWith` rt
               Nothing @=? fmap fst lookup -- avoids problem with TQueue not being and instance of Show
