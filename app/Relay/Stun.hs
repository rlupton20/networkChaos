{-# LANGUAGE OverloadedStrings #-} -- temporary
module Relay.Stun
( ) where

import Relay.Connection

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.Stun
import Network.Stun.Internal

import Data.Serialize
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Timeout
import Control.Applicative

-- Temporary imports
import Data.String


-- List of stun servers
stuns :: [(String, PortNumber)]
stuns = [("stun.ekiga.net",3478), ("stun.schlund.de",3478)]

-- List of resolved addressses
stunads :: IO [SockAddr]
stunads = sequence $ map (uncurry resolveAddr) stuns

-- Uses a given socket to stun with. More useful than the
-- Network.Stun library functions.
stunOn :: Socket -> SockAddr -> [Integer] -> IO SockAddr
stunOn sock server timeouts = do
  brq <- bindRequest
  let tos = if null timeouts then defaults else timeouts
      msg = encode brq

  resp <- doSTUN msg sock server tos

  let reply = decode resp
      ext = fmap getExternal reply
  case ext of
    Right (Just ad) -> return ad
    _ -> error "Failed to decode an external address."
    
  where
    doSTUN msg sock server [] = error "STUN timed out."
    doSTUN msg sock server (to:tos) = do
      sendTo sock msg server
      rep <- timeout to $ recv sock 4096
      case rep of
        Just m -> return m
        Nothing -> doSTUN msg sock server tos

    defaults = [500000,1000000,2000000]


-- getExternal takes a STUN response and extracts the
-- external socket address of the request.
getExternal :: Message -> Maybe SockAddr
getExternal msg = xma <|> ma  -- Two possible encodings
  where
    ma = case findAttribute (messageAttributes msg) of
      Right [ad] -> Just (unMA ad)
      _ -> Nothing
    xma = case findAttribute (messageAttributes msg) of
      Right [xad] -> Just $ fromXorMappedAddress (transactionID msg) xad
      _ -> Nothing

-- MANY REASONS STUN MAY FAIL
-- SYMMETRIC NAT IS JUST ONE, PROBABLY UNECESSARY
-- TO TEST FOR
-- Test for symmetric NAT
-- Not robust wrt to repeated replies
-- Make use of transaction IDs?
{- symNAT :: Socket -> IO Bool
symNAT sock = do
  ads <- stunads
  let ad1 = ads!!0
      ad2 = ads!!1
  ext1 <- stunOn sock ad1 []
  ext2 <- stunOn sock ad2 []
  return $ not (ext1 == ext2) -}

-- Try and holepunch NAT
holepunch :: IO ()
holepunch = do
  sock <- socket AF_INET Datagram defaultProtocol
  sockad <- resolveAddr (show $ iNADDR_ANY) aNY_PORT
  bind sock sockad
 
  (stun:_) <- stunads

  ext <- stunOn sock stun []
  putStrLn $ "I am " ++ show ext

  putStrLn "IP:"
  ip <- getLine
  putStrLn "Port:"
  p' <- (fmap read getLine) :: IO Integer
  let p = fromIntegral p'

  corr <- resolveAddr ip p
  putStrLn $ show corr

  race_ (punch sock corr) (take sock)
  
  close sock

  where
    punch s c = do
      sendTo s (fromString $ "PUNCH") c
      threadDelay 1000000
      punch s c

    take s = do
      recv s 4096 >>= putStrLn.show
      take s
      
