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
spawn :: Manager () -> Manager SubManager
spawn man = do
  env <- environment
  subman <- subManLog
  liftIO $ mask $ \restore -> do
    a <- async (restore $ man `manage` env)
    a `makeMonitoredOn` subman
    restore (return $ SubManager a)

    where

      makeMonitoredOn :: Async () -> SubManagerLog -> IO ()
      makeMonitoredOn a subman = do
        let m = SubManager a        
        watched <- atomically $ do
          subs <- readTVar subman
          let subs' = fmap (m:) subs in
            subs' `seq` return subs'

        -- Note there is a possiblity our manager has been  killed, and
        -- all submanagers are killed, before we have the chance to add
        -- our new SubManager to the tracking list. If that happens, we
        -- must kill the new SubManager manually. Note that if the
        -- Manager and its SubManagers are killed between the above STM
        -- transaction and the manual killing below, that's fine, since
        -- our new SubManager must have been added to the list, and the
        -- built in clean up procedure will take care of it.
        case watched of
          Just _ -> return ()
          Nothing -> do cancel a >> waitCatch a >> return ()
          

-- |subManLog provides the current running list of submanagers of a
-- manager. Users of the Manager monad, should not be concerned with
-- these, so this function is not exported, and only used internally.
subManLog :: Manager SubManagerLog
subManLog = lift (asks submanagers)

-- |fromEnvironment provides the result of applying a passed function
-- to the current environment
fromEnvironment :: (Environment -> a) -> Manager a
fromEnvironment = asks

-- |environment provides the current environment.
environment :: Manager Environment
environment = ask
