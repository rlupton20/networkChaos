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
