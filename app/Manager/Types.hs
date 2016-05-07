module Manager.Types where

import Routing.RoutingTable
import Command.Types

import Control.Concurrent.Async
import Control.Concurrent.STM.TVar

import Control.Monad.Trans.Reader

data Environment = Environment { routingTable :: RoutingTable
                               , commandQueue :: CommandQueue }

data Submanager = Submanager { process :: Async () }
type SubmanagerLog = TVar (Maybe [Submanager])
data ManageCtl = ManageCtl { submanagers :: SubmanagerLog }
type Manager = ReaderT Environment (ReaderT ManageCtl IO)
