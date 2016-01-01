module Relay.Interface
( UDPConn
, UDPSock
, newUDPSocket
, getSocket
, getSockAddr
, sockToConn ) where

import Relay.Connection
import Utils

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as B

-- |UDPConn represents a single UDP socket with a prescribed
-- corresponding socket to communicate with.
data UDPConn = UDPConn (Socket, SockAddr) SockAddr deriving (Show)

instance Connection UDPConn where
  receiveOn con@(UDPConn (sock, _) corr) = do
    (message, recp) <- recvFrom sock 8192
    if recp == corr then return message
       else putStrLn ("Bad client" ++ show recp) >> receiveOn con
    return message
    
  sendOn str (UDPConn (sock, _) corr) = do
    sent <- sendTo sock str corr
    putStrLn $ show sent ++ " bytes sent"
    return ()
    
  closeConn (UDPConn (sock,_) _) = do
    close sock
    return ()

-- |UDPSock is a type for a UDP socket to build a UDPConn on top of.
-- To link instances of the software, the port of the sockets is
-- required in advance.
newtype UDPSock = UDPSock (Socket, SockAddr) deriving (Eq, Show)

-- |sockToConn takes a UDPSock, and a correspondence IP address
-- and port, and builds a UDPConn from them.
sockToConn :: UDPSock -> (String, String) -> IO UDPConn
sockToConn (UDPSock udpsock) (corr, corrPort) = do
  destPort <- (readM corrPort :: IO Int)
  let portNum = fromIntegral destPort
  corAdd <- resolveAddr corr portNum
  return (UDPConn udpsock corAdd)

-- |newUDPSocket returns a new UDPSock structure, with the
-- underlying socket bound to a random port.
newUDPSocket :: IO UDPSock
newUDPSocket = do
  sock <- socket AF_INET Datagram defaultProtocol
  addr <- resolveAddr (show $ iNADDR_ANY) aNY_PORT
  bind sock addr
  sockaddr <- getSocketName sock
  return $ UDPSock (sock,sockaddr)

-- |getSocket returns the underlying socket of a UDPSock.
getSocket :: UDPSock -> Socket
getSocket (UDPSock (sock,_)) = sock

-- |getSockAddr returns the address bound to the underlying
-- socket of a UDPSock.
getSockAddr :: UDPSock -> SockAddr
getSockAddr (UDPSock (_,addr)) = addr
