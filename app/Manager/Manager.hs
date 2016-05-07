module Manager.Manager where

import Manager.Types
import Manager.Manage (manage)

import Routing.RoutingTable
import Command.Types

import Control.Concurrent.STM
import Control.Concurrent.Async

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Control.Exception

-- |makeManaged takes a RoutingTable, and creates a fresh
-- environment with which it can be managed.
makeManaged :: RoutingTable -> IO Environment
makeManaged rt = do
  cq <- newCommandQueue
  return $ Environment rt cq

-- |spawn creates a new manager in a new thread. If the new manager
-- crashes, it is caught by cull, and the exception is not propogated.
-- Each spawned submanager has its own new collection of submanagers,
-- and its own culling thread. This is intended to create independent
-- submanagers --- if the parent manager wants a result back from the
-- submanager then another method should be used, e.g. Async.
-- The submanager is of course responsible for leaving resources in a
-- consistent state in the event of an exception.
spawn :: Manager () -> Manager Submanager
spawn manager = do
  env <- environment
  sml <- submanagerLog
  liftIO $ mask $ \restore -> do
    a <- async (restore $ manager `manage` env)
    let m = Submanager a
    sml `track` m
    restore (return $ Submanager a)
    where
      track :: SubmanagerLog -> Submanager -> IO ()
      track sml m = atomically $ modifyTVar' sml $ fmap (m:)
  {- There is some subtlty to the operation here.

     Note that the contents of the SubManagerLog TVar can only
     be set to Nothing, if an exception has been hit or thrown to
     the Manager and execution has moved to the handler. Exceptions
     are masked, so this can only happen if an exception is thrown
     on a blocked operation. The documentation Control.Exception
     guarantees that STM transactions which don't use retry are
     guaranteed to be uniterruptible. In sum this means that once
     the new Async has been created, the SubManagerLog TVar must
     contain data of form Just [SubManager] and hence our new
     submanager is tracked before the next exception strikes.

     In other words, there is no chance of leaking the new thread.

     If understanding is required, it is worth simultaneously checking
     how manage behaves, and how it handles exceptions. -}



-- |subManagerLog provides the current running list of submanagers of a
-- manager. Users of the Manager monad, should not be concerned with
-- these, so this function is not exported, and only used internally.
submanagerLog :: Manager SubmanagerLog
submanagerLog = lift (asks submanagers)

-- |fromEnvironment provides the result of applying a passed function
-- to the current environment
fromEnvironment :: (Environment -> a) -> Manager a
fromEnvironment = asks

-- |environment provides the current environment.
environment :: Manager Environment
environment = ask
