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
import Control.Exception (try, getMaskingState, MaskingState(..))

import Manager

import Routing.RoutingTable

managerTest :: TF.Test
managerTest = testGroup "Manager.hs tests" $ hUnitTestToTests $ HU.TestList [ managerUnitTests ]

managerUnitTests :: HU.Test
managerUnitTests = HU.TestLabel "Basic manager unit tests" $ HU.TestList [ manageTest, postSpawnMaskingState ]

manageTest :: HU.Test
manageTest = "manage: launches a manager and returns" ~: test
  where
    test = do
      env <- makeManaged duffRoutingTable
      r <- (return ()) `manage` env
      () @=? r

postSpawnMaskingState :: HU.Test
postSpawnMaskingState = "spawn: check masking state is Unmasked after spawn" ~: test
  where
    test = do
      env <- makeManaged duffRoutingTable
      manager `manage` env

    manager = do
      spawn $ liftIO (threadDelay 1000000)
      liftIO $ do
        ms <- getMaskingState
        Unmasked @=? ms
      

duffRoutingTable :: RoutingTable
duffRoutingTable = undefined
