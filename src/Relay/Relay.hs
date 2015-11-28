module Relay.Relay
( makeRelay ) where

import Relay.Connection
  
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Concurrent.Async
import qualified Data.ByteString as B

makeRelay :: (Connection a) =>  a -> TQueue B.ByteString -> IO (TQueue B.ByteString)
makeRelay con inbound = do
  outbound <- newTQueueIO
  forkFinally (race_ (outbound `outOn` con) (inbound `inFrom` con)) (\_ -> closeConn con)
  return outbound

outOn :: (Connection a) => TQueue B.ByteString -> a -> IO ()
outOn q conn = loop
  where
    loop = do
      bs <- atomically $ readTQueue q
      bs `sendOn` conn
      loop

inFrom :: (Connection a) => TQueue B.ByteString -> a -> IO ()
inFrom q conn = loop
  where
    loop = do
      bs <- receiveOn conn
      atomically $ writeTQueue q bs
      loop
                    
