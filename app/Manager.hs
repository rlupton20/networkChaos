{-# LANGUAGE ExistentialQuantification #-}
module Manager
( Environment(..)
, Manager
, manage
, makeManaged
, spawn
, environment
, withEnvironment
, CommandQueue
, Command(..) ) where


import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar)
import Control.Concurrent.STM.TQueue ( TQueue, newTQueueIO
                                     , readTQueue, writeTQueue )

import Routing.RoutingTable ( RoutingTable, getAddr )
import Control.Concurrent.TreeThreads
import Control.Monad.IO.Class (liftIO)
import Network (Socket, PortNumber)

import Command.Types (Pending, newPending)

import Core

-- |Environment contains all the data that the Manager threads need
-- to be able to access.
data Environment = Environment { routingTable :: RoutingTable
                               , commandQueue :: CommandQueue
                               , pending :: Pending }

-- |Managers form a tree of threads which act on the Environment
type Manager = TreeThread Environment

-- A command is either a call to exit, or something which has an
-- interpretation in terms of a Manager.
data Command = Quit
             | Create Int (CommVar Connection)
             | Direct Connection (CommVar Connection)
             | Remove Addr


-- |makeManaged takes a RoutingTable, and creates a fresh
-- environment with which it can be managed.
makeManaged :: RoutingTable -> IO Environment
makeManaged table = do
  commands <- newQueue
  pen <- newPending
  return $ Environment table commands pen


-- |Reduce a Manager () to an IO ()
manage :: Manager () -> Environment -> IO ()
manage = sproutOn


--spawn :: Manager () -> Manager Branch
spawn = sprout

-- |CommandQueue is an abstraction of the programs internal list of
-- pending commands.
type CommandQueue = Queue Command
