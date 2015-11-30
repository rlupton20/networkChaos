{-# LANGUAGE OverloadedStrings #-}
module Relay.Interface
( UDPConn
, UDPSock
, newUDPSocket
, getSocket
, sockToConn
, addr ) where

import Relay.Connection

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as B

import Data.String

-- UDPPair is a standard pair of UDP sockets for sending and receiving;
-- hopefully later they will be replaced by encrypted sockets
data UDPConn = UDPConn (Socket, SockAddr) SockAddr deriving (Show)

instance Connection UDPConn where
  receiveOn (UDPConn (sock, _) corr) = do
    (message, recp) <- recvFrom sock 8192
    if recp == corr then return message else putStrLn ("Bad client" ++ show recp)>> (return $ fromString "")
    return message
  sendOn str (UDPConn (sock, _) corr) = do
    sent <- sendTo sock str corr
    putStrLn $ show sent ++ " bytes sent"
    return ()
  closeConn (UDPConn (sock,_) _) = do
    close sock
    return ()

addr :: UDPConn -> String
addr (UDPConn (_,ad) _) = show ad

-- UDPSock is a type for a UDP socket to build a UDPConn on top of.
-- To link instances of the software, the port of the sockets is
-- required in advance.

newtype UDPSock = UDPSock (Socket, SockAddr) deriving (Eq, Show)

sockToConn :: UDPSock -> (String, String) -> IO UDPConn
sockToConn (UDPSock sockAndAddr) (cor, corPort) = do
  let cp = fromIntegral (read corPort :: Int)
  corAdd <- resolveAddr cor cp
  return (UDPConn sockAndAddr corAdd)

newUDPSocket :: IO UDPSock
newUDPSocket = do
  sock <- socket AF_INET Datagram defaultProtocol

  addr <- resolveAddr (show $ iNADDR_ANY) aNY_PORT
  bind sock addr

  addrS <- getSocketName sock
  putStrLn $ "New socket on: " ++ show addrS
  
  return $ UDPSock (sock,addrS)

getSocket :: UDPSock -> Socket
getSocket (UDPSock (sock,_)) = sock

-- Utility function for resolving addresses
resolveAddr :: String -> PortNumber -> IO SockAddr
resolveAddr addr port = do
  addrInfo <- getAddrInfo Nothing (Just addr) (Just $ show port)
  let ads = map addrAddress addrInfo
  return $ head ads
