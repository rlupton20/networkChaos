{-# LANGUAGE OverloadedStrings #-}
module Relay.Connection
( Connection
, receiveOn
, sendOn 
, closeConn ) where

import qualified Data.ByteString as B
import Data.String

class Connection a where
  receiveOn :: a -> IO B.ByteString
  sendOn :: B.ByteString -> a -> IO ()
  closeConn :: a -> IO ()


