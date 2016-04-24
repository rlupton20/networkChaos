{-# LANGUAGE ExistentialQuantification, RankNTypes, RecordWildCards, DeriveDataTypeable #-}
module Manager.Manage
( manage ) where

import Manager.Types

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM

import Control.Exception

import Control.Monad
import Control.Monad.Trans.Reader

import Data.Typeable

data CullCrash = CullCrash deriving (Eq, Show, Typeable)
instance Exception CullCrash

-- |manage starts a manager process with an empty list of
-- submanagers, and launches a culling thread, which removes
-- completed submanagers from the tracking list.
manage :: Manager () -> Environment -> IO ()
manage m env = do
  subs <- newTVarIO $ Just []
  tid <- myThreadId
  
  mask $ \restore -> do
    cullProc <- async $ cullLoop restore tid subs
    r <- try (restore (runReaderT (runReaderT m env) $ (ManageCtl subs)))
    case r of
      -- If our manager has crashed with some exception, then we pass the
      -- exception to a handler to do the cleanup.
      Left e -> handleException e subs cullProc
      -- Otherwise, our manager ran successfuly, and there is nothing left
      -- to do, so cleanup any remaining child threads.
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


    -- handleException has the task of cleaning up. There are two cases:
    -- 1) either our culling thread crashed and sent us an exception, in
    --    which case cullProc is already dealing with an exception and we
    --    just need to wait for it to do its cleanup.
    -- 2) something went wrong in the manager itself, and cullProc doesn't
    --    know about this, so we need to cancel the cullProc thread manually,
    --    and wait for it to do cleanup.
    -- We deal with these two cases separately. In both cases, the submanagers
    -- need cleaning up.
    handleException :: SomeException ->
                       SubManagerLog ->
                       Async () ->
                       IO ()
    handleException e subs cullProc = do 
      killSubManagers subs
      let ex = fromException e :: Maybe CullCrash
      case ex of
        Just _ -> {- The exception was a CullCrash -} waitCatch cullProc >> return ()
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
