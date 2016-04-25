import Test.Framework
import Test.Framework.Providers.HUnit

import TypesTest
import Routing.RoutingTableTest

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testGroup "Types.hs" $ hUnitTestToTests typesTest
        , testGroup "RoutingTable.hs" $ hUnitTestToTests routingTableTest ]
