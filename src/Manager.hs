{-# LANGUAGE ExistentialQuantification, RankNTypes, RecordWildCards #-}
module Manager
( Environment(..)
, makeManaged
, Manager
, manage
, environment
, fromEnvironment
{-, tryManager
, maskManager-} ) where

import Routing.RoutingTable
import Command.Types

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Control.Exception

import Control.Monad

data Environment = Environment { routingTable :: RoutingTable
                           , commandQueue :: CommandQueue }

makeManaged :: RoutingTable -> IO Environment
makeManaged rt = do
  cq <- newCommandQueue
  return $ Environment rt cq

data SubManager = SubManager { subManagerThreadId :: ThreadId
                             , waitPoint :: TMVar (Either SomeException ()) }

data ManageCtl = ManageCtl { submanagers :: TMVar (Maybe [SubManager]) }

type Manager = ReaderT Environment (ReaderT ManageCtl IO)

manage :: Manager a -> Environment -> IO a
manage m env = do
  subs <- newTMVarIO $ Just []
  (runReaderT (runReaderT m $ env) $ (ManageCtl subs)) `finally`
    killSubManagers subs
    
  where
    
    killSubManagers :: TMVar (Maybe [SubManager]) -> IO ()
    killSubManagers subs = do
      submanagers <- getKillList subs
      sequence $ map kill submanagers
      sequence $ map (atomically.takeTMVar.waitPoint) submanagers
      return ()
      
    getKillList :: TMVar (Maybe [SubManager]) -> IO [SubManager]
    getKillList subs = atomically $ do
      msubs <- takeTMVar subs
      case msubs of
        Just toKill -> putTMVar subs Nothing >> return toKill
        Nothing -> {- Already killed -} return []

    kill :: SubManager -> IO ()
    kill SubManager{..} = throwTo subManagerThreadId ThreadKilled

                          
fromEnvironment :: (Environment -> a) -> Manager a
fromEnvironment = asks

environment :: Manager Environment
environment = ask

-- Exception handling stuff currently unused, and blocked out
-- for the moment --- it should be updated later

{-

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

-}
