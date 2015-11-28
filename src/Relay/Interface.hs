{-# LANGUAGE OverloadedStrings #-}
module Relay.Interface
( makeUDPPair ) where

import Relay.Connection

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as B

import Data.String

-- UDPPair is a standard pair of UDP sockets for sending and receiving;
-- hopefully later they will be replaced by encrypted sockets
data UDPPair = UDPPair (Socket, SockAddr) (Socket, SockAddr) (SockAddr, SockAddr) deriving (Show)

makeUDPPair :: SockAddr -> SockAddr -> IO UDPPair
makeUDPPair to from = do
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
  return $ UDPPair (inSock, inAddr) (outSock, outAddr) (to, from)

instance Connection UDPPair where
  receiveOn (UDPPair (inSock, _) _ (_,corr)) = do
    (message, recp) <- recvFrom inSock 8192
    if recp == corr then return message else putStrLn ("Bad client" ++ show recp)>> (return $ fromString "")
    return message
  sendOn str (UDPPair _ (outSock, _) (corr,_)) = do
    sent <- sendTo outSock str corr
    putStrLn $ show sent ++ " bytes sent"
    return ()
  closeConn (UDPPair (inSock,_) (outSock,_) _) = do
    close inSock
    close outSock
    return ()

-- Utility function for resolving addresses
resolveAddr :: String -> PortNumber -> IO SockAddr
resolveAddr addr port = do
  addrInfo <- getAddrInfo Nothing (Just addr) (Just $ show port)
  let ads = map addrAddress addrInfo
  return $ head ads
