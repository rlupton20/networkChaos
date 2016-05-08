module Relay.Relay
( makeRelay ) where

import Types  
import Relay.Connection

import Control.Monad
import Control.Concurrent.Async

-- |makeRelay takes a connection, and a Queue to put inbound packets,
-- and forks two sibling threads which can send and receive packets. It
-- returns a Queue on which to place outgoing packets. The connection
-- is closed in the event of an exception, or the failure of one of the
-- send or receive threads.
makeRelay :: (Connection a) =>  a -> Injector -> PacketQueue ->  IO ()
makeRelay con injector outbound = do
  race_ (outbound `outOn` con) (injector `inFrom` con)
  where
    
    outOn :: (Connection a) => PacketQueue -> a -> IO ()
    outOn q conn = forever $ do
      bs <- readQueue q
      bs `sendOn` conn

    inFrom :: (Connection a) => Injector -> a -> IO ()
    inFrom injector conn = forever $ do
      bs <- receiveOn conn
      bs `passTo` injector
                    
