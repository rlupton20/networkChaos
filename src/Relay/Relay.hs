module Relay.Relay
( makeRelay ) where

import ProcUnit
  
import Relay.Connection

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Concurrent.Async
import qualified Data.ByteString as B

-- |makeRelay takes a connection, and a TQueue to put inbound packets,
-- and forks two sibling threads which can send and receive packets. It
-- returns a TQueue one which to place outgoing packets. The connection
-- is closed in the event of an exception, or the failure of one of the
-- send or receive threads.
makeRelay :: (Connection a) =>  a -> ProcUnit B.ByteString () -> IO (TQueue B.ByteString)
makeRelay con injector = do
  outbound <- newTQueueIO
  -- Note, three threads per connection, one of which
  -- does very little. Reduce to two?
  forkFinally (race_ (outbound `outOn` con) (injector `inFrom` con)) (\_ -> closeConn con)
  return outbound
  where
    
    outOn :: (Connection a) => TQueue B.ByteString -> a -> IO ()
    outOn q conn = forever $ do
      bs <- atomically $ readTQueue q
      bs `sendOn` conn

    inFrom :: (Connection a) => ProcUnit B.ByteString () -> a -> IO ()
    inFrom injector conn = forever $ do
      bs <- receiveOn conn
      bs `passTo` injector
                    
