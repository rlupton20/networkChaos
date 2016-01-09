{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Manager
( Managed(..)
, makeManaged
, Manager
, manageWith
, ask
, asks
, tryManager
, maskManager ) where

import Routing.RoutingTable
import Command.Types

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Control.Exception

data Managed = Managed { routingTable :: RoutingTable
                       , commandQueue :: CommandQueue
                       , spawned :: TVar [ThreadId] }

makeManaged :: RoutingTable -> IO Managed
makeManaged rt = do
  cq <- newCommandQueue
  spwn <- newTVarIO []
  return $ Managed rt cq spwn

type Manager = ReaderT Managed IO

manageWith :: Manager a -> Managed -> IO a
manageWith m env = runReaderT m $ env

-- |tryReaderT is the try exception handler lifted to transformed
-- monads of type (ReaderT env IO a)
tryReaderT :: (Exception e) => ReaderT a IO b -> ReaderT a IO (Either e b)
tryReaderT action = do
              env <- ask
              liftIO $ try (runReaderT action $ env)

-- |tryManager is the try exception handler for the Manager monad.
tryManager :: (Exception e) => Manager a -> Manager (Either e a)
tryManager = tryReaderT

-- |maskReaderTIO allows us to mask operations from exceptions
-- inside monads of the shape (ReaderT e IO a).
maskReaderT :: ((forall a . ReaderT e IO a -> ReaderT e IO a) -> ReaderT e IO b) -> ReaderT e IO b
maskReaderT outline = do
  env <- ask
  liftIO $ mask $ \restore -> do
    let restoreM = \toUnmask -> do
          -- Since restore may be used inside local (or similar),
          -- we should ask for the environment every time we want
          -- to restore.
          locenv <- ask
          liftIO . restore $ (runReaderT toUnmask locenv)
          
    runReaderT (outline restoreM) $ env

-- |maskManager gives us masking in the Manager monad.
maskManager :: ((forall a . Manager a -> Manager a) -> Manager b) -> Manager b
maskManager = maskReaderT


