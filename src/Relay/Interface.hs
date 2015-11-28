{-# LANGUAGE OverloadedStrings #-}
module Relay.Interface
( relayPair ) where

import Relay.Connection

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as B

import Data.String

-- UDPPair is a standard pair of UDP sockets for sending and receiving;
-- hopefully later they will be replaced by encrypted sockets
data UDPPair = UDPPair (Socket, SockAddr) (Socket, SockAddr) (SockAddr, SockAddr) deriving (Show)

relayPair :: (String, String) -> (String, String) -> IO UDPPair
relayPair (to, toPort) (from, fromPort) = do
  let tp = fromIntegral (read toPort :: Int)
      fp = fromIntegral (read fromPort :: Int)
  toAdd <- resolveAddr to tp
  fromAdd <- resolveAddr from fp
  makeUDPPair toAdd fromAdd
  
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
