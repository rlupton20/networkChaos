{-# LANGUAGE ExistentialQuantification, RankNTypes, RecordWildCards, DeriveDataTypeable #-}
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
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Control.Exception
import Data.Typeable

import Control.Monad

data Environment = Environment { routingTable :: RoutingTable
                               , commandQueue :: CommandQueue }

makeManaged :: RoutingTable -> IO Environment
makeManaged rt = do
  cq <- newCommandQueue
  return $ Environment rt cq

data SubManager = SubManager { process :: Async ()}
type SubManagerLog = TVar (Maybe [SubManager])
data ManageCtl = ManageCtl { submanagers :: SubManagerLog }
type Manager = ReaderT Environment (ReaderT ManageCtl IO)

data CullCrash = CullCrash deriving (Eq, Show, Typeable)
instance Exception CullCrash

-- |manage starts a manager process with an empty list of
-- submanagers, and launches a culling thread, which removes
-- completed submanagers from the tracking list.
manage :: Manager a -> Environment -> IO ()
manage m env = do
  subs <- newTVarIO $ Just []
  tid <- myThreadId
  withAsyncWithUnmask (\restore -> cullLoop restore tid subs) $
           \cullProc -> do
             r <- try (runReaderT (runReaderT m env) $ (ManageCtl subs))
             case r of
               Left e -> handleException e subs cullProc
               Right _ -> do
                 cancel cullProc
                 killSubManagers subs
                 waitCatch cullProc
                 return ()
  where

    cullLoop :: (forall a. IO a -> IO a) ->
                ThreadId ->
                SubManagerLog ->
                IO ()
    cullLoop restore parentID subs = let tk = ThreadKilled in
      restore (forever $ cull subs) `catch`
        (\e -> if e == tk then throwIO e else do
            throwTo parentID CullCrash
            throwIO e)

    handleException :: SomeException ->
                       SubManagerLog ->
                       Async () ->
                       IO ()
    handleException e subs cullProc = do 
      killSubManagers subs
      let ex = fromException e :: Maybe CullCrash
      case ex of
        Just _ -> {- The exception was a CullCrash -} return ()
        Nothing -> cancel cullProc >> waitCatch cullProc >> return ()
      throwIO e
    
    killSubManagers :: SubManagerLog -> IO ()
    killSubManagers subs = do
      submanagers <- getKillList subs
      sequence $ map kill submanagers
      sequence $ map (waitCatch.process) submanagers
      return ()
      
    getKillList :: SubManagerLog -> IO [SubManager]
    getKillList subs = atomically $ do
      msubs <- swapTVar subs Nothing
      case msubs of
        Just toKill -> return toKill
        Nothing -> {- Already killed -} return []

    kill :: SubManager -> IO ()
    kill SubManager{..} = cancel process

-- |cull takes our tracked SubManagers, and returns a list of
-- completed submanagers, leaving behind only those that are
-- still running.
cull :: SubManagerLog -> IO (Maybe [SubManager])
cull tjsubs = atomically $ do
    jsubs <- readTVar tjsubs
    case jsubs of
      Nothing -> return Nothing
      Just subs -> do
        (done,working) <- divideSubManagers subs
        writeTVar tjsubs (Just working)
        return (Just done)

-- |divideSubManagers takes a list of SubManagers and returns two lists:
-- one a list of completed submanagers, and the other a list of those
-- still running. Ordering of lists is not guaranteed to be preserved.
-- Will block if nothing has finished running
divideSubManagers :: [SubManager] -> STM ([SubManager],[SubManager])
divideSubManagers subs = go [] [] subs
  where
    -- This is really a right fold, though it may be less clear
    -- expressed that way
    go [] _ [] = retry
    go cs rs [] = return (cs,rs)
    go cs rs (s:ss) = (attempt s cs rs ss) `orElse` go cs (s:rs) ss

    attempt s cs rs ss = do
      _ <- waitCatchSTM (process s)
      go (s:cs) rs ss

{- The foldr version of divideSubManagers. Higher level, but not necessarily clearer...
divideSubManagers subs = ((foldr doSplit retryOrDone) $ (map decide subs)) $ ([],[])
  where
    doSplit :: (([SubManager],[SubManager]) -> STM ([SubManager],[SubManager])) ->
               (([SubManager],[SubManager]) -> STM ([SubManager],[SubManager])) ->
               ([SubManager],[SubManager]) -> STM ([SubManager],[SubManager])
                             
    doSplit splitNext decideFirst subs = decideFirst subs >>= splitNext
    
    retryOrDone :: ([SubManager],[SubManager]) -> STM ([SubManager],[SubManager])
    retryOrDone split@(culls,_) = if null culls then retry else return split

    decide :: SubManager -> ([SubManager],[SubManager]) -> STM ([SubManager],[SubManager])
    decide s (cs,rs) = (waitCatchSTM (process s) >> return (s:cs,rs)) `orElse` return (cs, s:rs) -}

-- |spawn creates a new manager in a new thread. If the new manager
-- crashes, it is caught by cull, and the exception is not propogated.
spawn :: Manager a -> Manager ThreadId
spawn subman = undefined


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
