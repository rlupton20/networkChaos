{-# LANGUAGE OverloadedStrings #-}
module Relay.Interface
( UDPConn
, UDPSock
, newUDPSocket
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

type UDPSock = (Socket, SockAddr)

sockToConn :: UDPSock -> (String, String) -> IO UDPConn
sockToConn udpSock (cor, corPort) = do
  let cp = fromIntegral (read corPort :: Int)
  corAdd <- resolveAddr cor cp
  return (UDPConn udpSock corAdd)

newUDPSocket :: IO UDPSock
newUDPSocket = do
  sock <- socket AF_INET Datagram defaultProtocol

  addr <- resolveAddr (show $ iNADDR_ANY) aNY_PORT
  bind sock addr

  addrS <- getSocketName sock
  putStrLn $ "New socket on: " ++ show addrS
  
  return (sock,addrS)

addr :: UDPConn -> String
addr (UDPConn (_,ad) _) = show ad

-- Utility function for resolving addresses
resolveAddr :: String -> PortNumber -> IO SockAddr
resolveAddr addr port = do
  addrInfo <- getAddrInfo Nothing (Just addr) (Just $ show port)
  let ads = map addrAddress addrInfo
  return $ head ads
