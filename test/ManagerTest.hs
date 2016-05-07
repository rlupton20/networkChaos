
module ManagerTest
( managerTest ) where

import Test.Framework as TF
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit as HU
import Test.HUnit ((~:),(@=?))

import Control.Concurrent (threadDelay)
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
managerTest = testGroup "Manager.hs tests" $ hUnitTestToTests $ HU.TestList [ managerUnitTests, exceptionTests ]

managerUnitTests :: HU.Test
managerUnitTests = HU.TestLabel "Basic manager unit tests" $ HU.TestList [ manageTest, spawnTrackTest ]

manageTest :: HU.Test
manageTest = "manage: launches a manager and returns" ~: test
  where
    test = do
      env <- makeManaged duffRoutingTable
      r <- (return ()) `manage` env
      () @=? r

spawnTrackTest :: HU.Test
spawnTrackTest = "spawn: check spawned submanagers are tracked by the parent" ~: test
  where
    test = do
      env <- makeManaged duffRoutingTable
      manager `manage` env

    manager = do
      spawn $ liftIO (threadDelay 1000000)
      sml <- submanagerLog
      liftIO $ do
        subs <- atomically $ readTVar sml
        Just 1 @=? fmap length subs
        return ()
      

exceptionTests :: HU.Test
exceptionTests = HU.TestLabel "Exception infrastructure tests" $ HU.TestList []

duffRoutingTable :: RoutingTable
duffRoutingTable = undefined
