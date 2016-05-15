module Relay.Interface
( UDPConn
, UDPSock
, newUDPSocket
, getSocket
, getSockAddr
, sockToConn ) where

import Relay.Connection
import Utils

import qualified Network.Socket as Net hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as Net
import qualified Data.ByteString.Char8 as B

-- |UDPConn represents a single UDP socket with a prescribed
-- corresponding socket to communicate with.
data UDPConn = UDPConn (Net.Socket, Net.SockAddr) Net.SockAddr deriving (Show)

instance Connection UDPConn where
  receiveOn connection@(UDPConn (socket, _) correspondant) = do
    (message, endpoint) <- Net.recvFrom socket 8192
    if endpoint == correspondant then return message
       else putStrLn ("Bad client " ++ show endpoint) >> receiveOn connection
    return message
    
  sendOn bs (UDPConn (socket, _) correspondant) = do
    Net.sendTo socket bs correspondant
    return ()
    
  closeConn (UDPConn (socket,_) _) = do
    Net.close socket
    return ()

-- |UDPSock is a type for a UDP socket to build a UDPConn on top of.
-- To link instances of the software, the port of the sockets is
-- required in advance.
newtype UDPSock = UDPSock (Net.Socket, Net.SockAddr) deriving (Eq, Show)

-- |sockToConn takes a UDPSock, and a correspondence IP address
-- and port, and builds a UDPConn from them.
sockToConn :: UDPSock -> (String, String) -> IO UDPConn
sockToConn (UDPSock socket) (correspondant, port) = do
  destinationPort <- (readM port :: IO Int)
  let port' = fromIntegral destinationPort
  address <- resolveAddress correspondant port'
  return (UDPConn socket address)

-- |newUDPSocket returns a new UDPSock structure, with the
-- underlying socket bound to a random port.
newUDPSocket :: IO UDPSock
newUDPSocket = do
  socket <- Net.socket Net.AF_INET Net.Datagram Net.defaultProtocol
  address <- resolveAddress (show $ Net.iNADDR_ANY) Net.aNY_PORT
  Net.bind socket address
  socketAddress <- Net.getSocketName socket
  return $ UDPSock (socket,socketAddress)

-- |getSocket returns the underlying socket of a UDPSock.
getSocket :: UDPSock -> Net.Socket
getSocket (UDPSock (socket,_)) = socket

-- |getSockAddr returns the address bound to the underlying
-- socket of a UDPSock.
getSockAddr :: UDPSock -> Net.SockAddr
getSockAddr (UDPSock (_,address)) = address
