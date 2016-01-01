{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Environments
( Environment(..)
, Manager
, manageWith
, asks
, tryManager
, maskManager ) where

import Routing.RoutingTable

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Exception

import Control.Concurrent -- testing

data Environment = Environment { routingTable :: RoutingTable }

type Manager = ReaderT Environment IO

manageWith :: Manager a -> Environment -> IO a
manageWith m env = runReaderT m $ env

-- |tryReaderTIO is the try exception handler lifted to transformed
-- monads of type (ReaderT env IO a)
tryReaderTIO :: (Exception e) => ReaderT a IO b -> ReaderT a IO (Either e b)
tryReaderTIO action = do
              env <- ask
              liftIO $ try (runReaderT action $ env)

-- |tryManager is the try exception handler for the Manager monad.
tryManager :: (Exception e) => Manager a -> Manager (Either e a)
tryManager = tryReaderTIO

-- |maskReaderTIO allows us to mask operations from exceptions
-- inside monads of the shape (ReaderT e IO a).
maskReaderTIO :: ((forall a . ReaderT e IO a -> ReaderT e IO a) -> ReaderT e IO b) -> ReaderT e IO b
maskReaderTIO outline = do
  env <- ask
  liftIO $ mask $ \restore -> do
    -- Since restore may be used inside local, we should ask
    -- for the environment every time we want to restore
    let restoreM = \toUnmask -> do
          loc <- ask
          liftIO . restore $ (runReaderT toUnmask loc)
          
    runReaderT (outline restoreM) $ env

-- |maskManager gives us masking in the Manager monad.
maskManager :: ((forall a . Manager a -> Manager a) -> Manager b) -> Manager b
maskManager = maskReaderTIO
