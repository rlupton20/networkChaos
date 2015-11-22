{-# LANGUAGE OverloadedStrings #-}
module Connection
( Connection
, receive
, Connection.send 
, StdIO(StdIO)
, makeUDPPair
, closeUDPPair ) where

import qualified Data.ByteString.Char8 as B
import Data.String

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

class Connection a where
  receive :: a -> IO B.ByteString
  send :: a -> B.ByteString -> IO ()

-- StdIO is a debugging connection, which simply makes use of the
-- standard input and output
data StdIO = StdIO deriving (Show)

instance Connection StdIO where
  receive a = B.getLine
  send a str = B.putStrLn $ str

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
  receive (UDPPair (inSock, _) _ (corr,_)) = do
    (message, recp) <- recvFrom inSock 8192
    --if recp == corr then return message else putStrLn ("Bad client" ++ show recp)>> (return $ fromString "")
    return message
  send (UDPPair _ (outSock, _) (_,corr)) str = do
    sent <- sendTo outSock str corr
    --putStrLn $ show sent ++ " bytes sent"
    return ()

-- Utility function for resolving addresses
resolveAddr :: String -> PortNumber -> IO SockAddr
resolveAddr addr port = do
  addrInfo <- getAddrInfo Nothing (Just addr) (Just $ show port)
  let ads = map addrAddress addrInfo
  return $ head ads
