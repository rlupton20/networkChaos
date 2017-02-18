module Command.STUN where

import Network.Socket (Socket, SockAddr(..), PortNumber, hostAddressToTuple)
import Network.Socket.ByteString (sendTo, recv)
import Control.Applicative ((<|>))

import Network.Stun
import Network.Stun.Internal
import Data.Serialize
import Control.Concurrent.Timeout (timeout)

import Core


-- List of stun servers
stunserver :: (String, Maybe PortNumber)
stunserver = ("stun.ekiga.net", Just 3478)

-- Uses a given socket to stun with. More useful than the
-- Network.Stun library functions.
stunOn :: Socket -> SockAddr -> IO (Maybe (Addr, PortNumber))
stunOn sock server = do
  brq <- bindRequest
  let msg = encode brq

  resp <- doSTUN msg sock server timeouts
  case resp of
    Just m -> do
      let reply = decode m
          ext = fmap getExternal reply
      case ext of
        Right address@(Just _) -> return $ address >>= decompose
        _ -> return Nothing
    _ -> return Nothing
    
  where
    doSTUN msg sock server [] = return Nothing
    doSTUN msg sock server (to:tos) = do
      sendTo sock msg server
      rep <- timeout to $ recv sock 4096
      case rep of
        m@(Just _ ) -> return m
        Nothing -> doSTUN msg sock server tos

    timeouts = [500000,1000000,2000000]


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
