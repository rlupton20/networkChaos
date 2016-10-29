module Manager
( Environment(..)
, Manager
, manage
, makeManaged
, spawn
, environment
, withEnvironment ) where


import Command.Types ( CommandQueue, newCommandQueue )
import Routing.RoutingTable ( RoutingTable )
  
import Control.Concurrent.TreeThreads


data Environment = Environment { routingTable :: RoutingTable
                               , commandQueue :: CommandQueue }


type Manager = TreeThread Environment

  
-- |makeManaged takes a RoutingTable, and creates a fresh
-- environment with which it can be managed.
makeManaged :: RoutingTable -> IO Environment
makeManaged table = do
  commands <- newCommandQueue
  return $ Environment table commands

 
manage :: Manager () -> Environment -> IO ()
manage = sproutOn


--spawn :: Manager () -> Manager Branch
spawn = sprout
