module ManagerTest
( managerTest ) where

import Test.Framework as TF
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit as HU
import Test.HUnit ((~:),(@=?))

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try)

import Data.Time.Clock (getCurrentTime)
import System.Timeout (timeout)

import Manager.Manager
import Manager.Manage
import Manager.Types

import Routing.RoutingTable

managerTest :: TF.Test
managerTest = testGroup "Manager.hs tests" $ hUnitTestToTests exceptionTests

exceptionTests :: HU.Test
exceptionTests = HU.TestLabel "Exception infrastructure tests" $ HU.TestList []

-- The following would be good to test (crashing the culling thread and then
-- checking the exception propagates, but I'm not sure if / how it can be done.
-- The commented out code is broken.
{-
-- |Force the SubManager culling thread to crash by placing a bogus SubManager
-- in the SubManager log. The exception should cause the culling thread to die,
-- and an appropriate exception to be thrown to the Manager thread, which we check.
cullCrashTest :: HU.Test
cullCrashTest = "Check that the culling thread crashing propagates to the Manager:" ~: test
  where
    test = do
      env <- makeManaged duffRoutingTable
      cv <- (timeout 500) . try $ doomedManager `manage` env
      Just (Left CullCrash) @=? cv
      
    doomedManager :: Manager ()
    doomedManager = do
      t <- liftIO $ getCurrentTime >>= newTVarIO -- We use this to fake some work later

      -- Now lets put a bad SubManager in the SubManagerLog
      sml <- subManLog
      liftIO $ do
        atomically $ modifyTVar' sml (fmap (brokenSubManager:))

        -- This should crash the culling thread, and hence the Manager. In
        -- the meantime do some pointless but interruptible work.
        forever $ do
          time <- getCurrentTime
          atomically $ modifyTVar' t $ (const time)
      
-}

brokenSubManager :: SubManager
brokenSubManager = SubManager $ error "Forced cull crash."

duffRoutingTable :: RoutingTable
duffRoutingTable = undefined
