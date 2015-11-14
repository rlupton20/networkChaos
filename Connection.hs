{-# LANGUAGE OverloadedStrings #-}
module Connection where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.TLS

import Data.ByteString as B
import Data.String

import Control.Monad.IO.Class

class Connection a where
  dispatchTo :: (MonadIO m) => B.ByteString -> a -> m ()
  collectFrom :: (MonadIO m) => a -> m B.ByteString

-- As a test example of a "connection", lets use
-- the standard input and output
data StdIO = StdIO
instance Connection StdIO where
  dispatchTo str _ = liftIO . B.putStrLn $ str
  collectFrom _ = liftIO $ B.getLine

-- Data type representing two plain (TCP) sockets for two
-- way communication. Are the data structures correct?
-- IDEA: alter Connection datatype or put checking in instances?
data DualSockets = DualSockets Socket Socket
instance Connection DualSockets where
  dispatchTo str (DualSockets _ outSock) = liftIO $ do
    send outSock str
    return ()
  collectFrom (DualSockets inSock _) = liftIO $ do
    fmap fst $ recvFrom inSock 4096
