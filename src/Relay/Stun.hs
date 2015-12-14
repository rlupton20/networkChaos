module Relay.Stun
( ) where

import Relay.Connection
import Network.Socket
import Network.Stun
import Network.Stun.Internal

import Control.Applicative

stunServer = "stun.ekiga.net"

-- stun returns a socket with its external address
stun :: IO (Socket, SockAddr)
stun = do
  brq <- bindRequest
  stunAddr <- resolveAddr stunServer 3478
  res <- stunRequest' stunAddr 0 [] brq
  
  (msg, sock) <- case res of
    Right (msg, sock) -> return (msg, sock)
    Left e -> error $ show e

  let ext = getExternal msg

  case ext of
       Just ad -> return (sock, ad)
       _ -> error "Couldn't STUN."


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
