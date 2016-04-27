import Test.Framework
import Test.Framework.Providers.HUnit

import TypesTest
import Routing.RoutingTableTest
import ManagerTest

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ typesTest
        , routingTableTest
        , managerTest ]
