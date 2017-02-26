import Test.Framework
import Test.Framework.Providers.HUnit

import Routing.RoutingTableTest (routingTableTest)
import ManagerTest (managerTest)
import ConfigTest (configTest)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ routingTableTest
        , managerTest
        , configTest ]
