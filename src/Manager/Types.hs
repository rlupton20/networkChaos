module Manager.Types where

import Routing.RoutingTable
import Command.Types

import Control.Concurrent.Async
import Control.Concurrent.STM.TVar

import Control.Monad.Trans.Reader

data Environment = Environment { routingTable :: RoutingTable
                               , commandQueue :: CommandQueue }

data SubManager = SubManager { process :: Async ()}
type SubManagerLog = TVar (Maybe [SubManager])
data ManageCtl = ManageCtl { submanagers :: SubManagerLog }
type Manager = ReaderT Environment (ReaderT ManageCtl IO)
