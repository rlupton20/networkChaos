module Relay.Interfaces
( StdIO(StdIO)
, makeUDPPair ) where

import Relay.Connection

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as B

-- StdIO is a debugging connection, which simply makes use of the
-- standard input and output
data StdIO = StdIO deriving (Show)

instance Connection StdIO where
  receiveOn a = B.getLine
  sendOn str a = B.putStrLn $ str
  closeConn _ = return ()

-- UDPPair is a standard pair of UDP sockets for sending and receiving;
-- hopefully later they will be replaced by encrypted sockets
data UDPPair = UDPPair (Socket, SockAddr) (Socket, SockAddr) (SockAddr, SockAddr) deriving (Show)

makeUDPPair :: SockAddr -> IO UDPPair
makeUDPPair _ = do
  inSock <- socket AF_INET Datagram defaultProtocol
  outSock <- socket AF_INET Datagram defaultProtocol

  inAddr <- resolveAddr (show $ iNADDR_ANY) aNY_PORT
  outAddr <- resolveAddr (show $ iNADDR_ANY) aNY_PORT
  bind inSock inAddr
  bind outSock outAddr

  inAddr <- getSocketName inSock
  outAddr <- getSocketName outSock
  putStrLn $ "In on " ++ show inAddr ++ "; Out on " ++ show outAddr

  -- The final tuple is at the moment just a placeholder;
  -- it should eventually contain the corresponding addresses
  -- being sent to and received from
  return $ UDPPair (inSock, inAddr) (outSock, outAddr) (outAddr, inAddr)
  
closeUDPPair :: UDPPair -> IO ()
closeUDPPair (UDPPair (inSock,_) (outSock,_) _) = do
  close inSock
  close outSock
  return ()

instance Connection UDPPair where
  receiveOn (UDPPair (inSock, _) _ (corr,_)) = do
    (message, recp) <- recvFrom inSock 8192
    --if recp == corr then return message else putStrLn ("Bad client" ++ show recp)>> (return $ fromString "")
    return message
  sendOn str (UDPPair _ (outSock, _) (_,corr)) = do
    sent <- sendTo outSock str corr
    --putStrLn $ show sent ++ " bytes sent"
    return ()
  closeConn = closeUDPPair

-- Utility function for resolving addresses
resolveAddr :: String -> PortNumber -> IO SockAddr
resolveAddr addr port = do
  addrInfo <- getAddrInfo Nothing (Just addr) (Just $ show port)
  let ads = map addrAddress addrInfo
  return $ head ads
