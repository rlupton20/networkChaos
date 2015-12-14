module Relay.Stun
( ) where

import Relay.Connection
import Network.Socket
import Network.Stun
import Network.Stun.Internal

import Control.Applicative

stunServer = "stun.ekiga.net"

-- stun returns a socket with its external address
-- This isn't robust yet...
stun :: IO (Socket, SockAddr)
stun = do
  brq <- bindRequest
  stunAddr <- resolveAddr stunServer 3478
  -- stunr <- findMappedAddress stunAddr 0 []
  Right (msg, sock) <- stunRequest' stunAddr 0 [] brq
  let ext = getExternal msg
  case ext of
       Just ad -> return (sock, ad)
       _ -> error "Couldn't STUN."

getExternal :: Message -> Maybe SockAddr
getExternal msg = xma <|> ma
  where
    ma = case findAttribute (messageAttributes msg) of
      Right [ad] -> Just (unMA ad)
      _ -> Nothing
    xma = case findAttribute (messageAttributes msg) of
      Right [xad] -> Just $ fromXorMappedAddress (transactionID msg) xad
      _ -> Nothing
