module Relay.Relay
( makeRelay ) where


import Control.Monad (forever)
import Control.Concurrent.Async (race_)

import Types  
import Relay.Connection

-- |makeRelay takes a connection, a Queue on which to put inbound
-- packets (an Injector), and a Queue on which to put outbound packets.
-- It then forks two sibling threads, one of which receives packets
-- and puts them on the inbound Queue (Injector), and the other which
-- takes packets off of the outbound Queue and sends them. If one of the
-- threads terminates, so does the other.
makeRelay :: (Connection a) =>  a -> Injector -> PacketQueue ->  IO ()
makeRelay connection injector outbound = do
  race_ (outbound `outOn` connection) (injector `inFrom` connection)
  where
    outOn :: (Connection a) => PacketQueue -> a -> IO ()
    outOn q connection = forever $ do
      bs <- readQueue q
      bs `sendOn` connection

    inFrom :: (Connection a) => Injector -> a -> IO ()
    inFrom injector connection = forever $ do
      bs <- receiveOn connection
      bs `passTo` injector
                    
