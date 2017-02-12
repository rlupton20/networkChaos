{-# LANGUAGE OverloadedStrings #-}
module ControlTest ( controlTest ) where

import Test.Framework as TF
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit as HU
import Test.HUnit ((~:),(@=?))

import Data.Aeson (decode)
import Data.ByteString.Lazy (ByteString, append)


import Command.Control
import Core

controlTest :: TF.Test
controlTest = testGroup "Control.hs tests" $ hUnitTestToTests $
               HU.TestList [ testRejectsBadRequest
                           , testCanParseNewConnection
                           , testRejectsBadDataOnNewConnection ]

testRejectsBadRequest :: HU.Test
testRejectsBadRequest = "Request: check bad request is recognized" ~: test
  where
    test = let json = "{\"this is bad json\" : [1,2,3,4] }"
               expected = BadRequest in
             Just expected @=? decode json


testCanParseNewConnection :: HU.Test
testCanParseNewConnection = "New: Can parse new connection command" ~: test
  where
    test = let json = "{ \"request\" : \"new\", " `append`
                        "\"endpoint\" : {" `append`
                          "\"virtualip\" : [1,2,211,56]," `append`
                          "\"ip\": [44,33,22,11]," `append`
                          "\"port\": 678 }}"
               (Just vip) = addr "1.2.211.56"
               (Just ip) = addr "44.33.22.11"
               port = 678
               expected = New $ Connection vip ip port in
             Just expected @=? decode json
                         

testRejectsBadDataOnNewConnection :: HU.Test
testRejectsBadDataOnNewConnection = "New: Reject bad data on new connection" ~: test
  where
    test = let json = "{ \"request\" : \"new\", " `append`
                        "\"endpoint\" : 5 }"
               expected = BadRequest in
             Just expected @=? decode json
