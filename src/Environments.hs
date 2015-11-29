module Environments
( Environment(..)
, Manager
, manageWith ) where

import Routing.RoutingTable

import Control.Monad.Trans.Reader

data Environment = Environment { routingTable :: RoutingTable
                               , other :: String }

type Manager = ReaderT Environment IO

manageWith :: Manager a -> Environment -> IO a
manageWith m env = runReaderT m $ env

