import Test.Framework
import Test.Framework.Providers.HUnit

import TypesTest (typesTest)
import Routing.RoutingTableTest (routingTableTest)
import ManagerTest (managerTest)
import ConfigTest (configTest)
import OracleTest (oracleTest)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ typesTest
        , routingTableTest
        , managerTest
        , configTest
        , oracleTest ]
