module Relay.Connection
( Connection
, receiveOn
, sendOn 
, closeConn
, resolveAddress  ) where

import qualified Data.ByteString as B
import qualified Network.Socket as S
import Network.Socket (PortNumber, SockAddr)

class Connection a where
  receiveOn :: a -> IO B.ByteString
  sendOn :: B.ByteString -> a -> IO ()
  closeConn :: a -> IO ()

-- Utility function for resolving addresses
resolveAddress :: String -> PortNumber -> IO SockAddr
resolveAddress addr port = do
  addrInfo <- S.getAddrInfo Nothing (Just addr) (Just $ show port)
  let ads = map S.addrAddress addrInfo
  return $ head ads
