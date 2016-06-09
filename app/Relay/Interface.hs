module Relay.Interface
( UDPConnection
, UDPSocket
, newUDPSocket
, getSocket
, getSockAddr
, sockToConn ) where


import qualified Network.Socket as Net hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as Net
import qualified Data.ByteString.Char8 as B

import Relay.Connection
import Utils

  
-- |UDPConnection represents a single UDP socket with a prescribed
-- corresponding socket to communicate with.
data UDPConnection = UDPConnection (Net.Socket, Net.SockAddr) Net.SockAddr deriving (Show)

instance Connection UDPConnection where
  receiveOn connection = do
    let (UDPConnection (socket, _) correspondant) = connection
    (message, endpoint) <- Net.recvFrom socket 8192
    if endpoint == correspondant then return message
       else putStrLn ("Bad client " ++ show endpoint) >> receiveOn connection
    return message
    
  sendOn bs (UDPConnection (socket, _) correspondant) = do
    Net.sendTo socket bs correspondant
    return ()
    
  closeConn (UDPConnection (socket,_) _) = do
    Net.close socket
    return ()

-- |UDPSocket is a type for a UDP socket to build a
-- UDPConnection on top of.
-- To link instances of the software, the port of the sockets is
-- required in advance.
newtype UDPSocket = UDPSocket (Net.Socket, Net.SockAddr) deriving (Eq, Show)

-- |sockToConn takes a UDPSocket, and a correspondence IP address
-- and port, and builds a UDPConnection from them.
sockToConn :: UDPSocket -> (String, String) -> IO UDPConnection
sockToConn (UDPSocket socket) (correspondant, port) = do
  destinationPort <- (readM port :: IO Int)
  let port' = fromIntegral destinationPort
  address <- resolveAddress correspondant port'
  return (UDPConnection socket address)

-- |newUDPSocket returns a new UDPSocket structure, with the
-- underlying socket bound to a random port.
newUDPSocket :: IO UDPSocket
newUDPSocket = do
  socket <- Net.socket Net.AF_INET Net.Datagram Net.defaultProtocol
  address <- resolveAddress (show $ Net.iNADDR_ANY) Net.aNY_PORT
  Net.bind socket address
  socketAddress <- Net.getSocketName socket
  return $ UDPSocket (socket,socketAddress)

-- |getSocket returns the underlying socket of a UDPSocket.
getSocket :: UDPSocket -> Net.Socket
getSocket (UDPSocket (socket,_)) = socket

-- |getSockAddr returns the address bound to the underlying
-- socket of a UDPSocket.
getSockAddr :: UDPSocket -> Net.SockAddr
getSockAddr (UDPSocket (_,address)) = address
