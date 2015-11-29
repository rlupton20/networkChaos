module Environments
( Environment(..)
, Manager
, manageWith
, asks
, tryM ) where

import Routing.RoutingTable

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Exception

data Environment = Environment { routingTable :: RoutingTable }

type Manager = ReaderT Environment IO

manageWith :: Manager a -> Environment -> IO a
manageWith m env = runReaderT m $ env

tryM :: (Exception e) => Manager a -> Manager (Either e a)
tryM action = do
              env <- ask
              liftIO $ try (action `manageWith` env)
