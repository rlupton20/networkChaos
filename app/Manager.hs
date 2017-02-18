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
, Command(..)
, getCommand
, postCommand
, newCommandQueue ) where


import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar)
import Control.Concurrent.STM.TQueue ( TQueue, newTQueueIO
                                     , readTQueue, writeTQueue )

import Routing.RoutingTable ( RoutingTable, getAddr )
import Control.Concurrent.TreeThreads
import Control.Monad.IO.Class (liftIO)

import Command.Types (Connection(..), Pending, newPending)
import Core (Addr, describeSocket)
import Network (Socket, PortNumber)


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
             | Create Int (TMVar (Addr, PortNumber))
             | Direct Addr (Addr, PortNumber) (TMVar Connection)
             | Remove Addr

-- |makeManaged takes a RoutingTable, and creates a fresh
-- environment with which it can be managed.
makeManaged :: RoutingTable -> IO Environment
makeManaged table = do
  commands <- newCommandQueue
  pen <- newPending
  return $ Environment table commands pen


-- |Reduce a Manager () to an IO ()
manage :: Manager () -> Environment -> IO ()
manage = sproutOn


--spawn :: Manager () -> Manager Branch
spawn = sprout

-- |CommandQueue is an abstraction of the programs internal list of
-- pending commands.
data CommandQueue = CommandQueue (TQueue Command)

-- |getCommand is a utility wrapper for fetching the next command
getCommand :: CommandQueue -> IO Command
getCommand cq = let (CommandQueue q) = cq in atomically $ readTQueue q

-- |postCommand is a utility wrapper for posting a command
postCommand :: CommandQueue -> Command -> IO ()
postCommand cq c = let (CommandQueue q) = cq in atomically $ writeTQueue q c

-- |newCommandQueue provides us with a new CommandQueue
newCommandQueue :: IO CommandQueue
newCommandQueue = do
  q <- newTQueueIO
  return (CommandQueue q)
