import Test.Framework

import Routing.RoutingTableTest (routingTableTest)
import ManagerTest (managerTest)
import ConfigTest (configTest)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ routingTableTest
        , managerTest
        , configTest ]
