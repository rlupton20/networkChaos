import Test.Framework
import Test.Framework.Providers.HUnit

import TypesTest (typesTest)
import Routing.RoutingTableTest (routingTableTest)
import ManagerTest (managerTest)
import ConfigTest (configTest)
import ControlTest (controlTest)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ typesTest
        , routingTableTest
        , managerTest
        , controlTest
        , configTest ]
