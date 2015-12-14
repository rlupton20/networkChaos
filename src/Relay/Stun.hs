module Relay.Stun
( ) where

import Relay.Connection

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.Stun
import Network.Stun.Internal

import Data.Serialize
import Control.Concurrent.Timeout
import Control.Applicative

-- Some test server for STUNning
stunServer = "stun.ekiga.net"
stunPort = 3478

--otherStun = "stun.schlund.de"

-- Uses a given socket to stun with. More useful than the
-- Network.Stun library functions.
stunOn :: Socket -> SockAddr -> [Integer] -> IO SockAddr
stunOn sock server timeouts = do
  brq <- bindRequest
  let tos = if null timeouts then [500000,1000000,2000000] else timeouts
      msg = encode brq

  resp <- doSTUN msg sock server tos

  let reply = decode resp
      ext = fmap getExternal reply
  case ext of
    Right (Just ad) -> return ad
    _ -> error "testSock failed."
    
  where
    doSTUN msg sock server [] = error "STUN timed out."
    doSTUN msg sock server (to:tos) = do
      sendTo sock msg server
      rep <- timeout to $ recv sock 4096
      case rep of
        Just m -> return m
        Nothing -> doSTUN msg sock server tos


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
