module Relay.Stun
( ) where

import Relay.Connection
import Network.Socket
import Network.Stun
import Network.Stun.Internal

stunServer = "stun.ekiga.net"

-- stun returns a socket with its external address
-- This isn't robust yet...
stun :: IO (Socket, SockAddr)
stun = do
  brq <- bindRequest
  stunAddr <- resolveAddr stunServer 3478
  -- stunr <- findMappedAddress stunAddr 0 []
  Right (msg, sock) <- stunRequest' stunAddr 0 [] brq
  let ma = (findAttribute ( messageAttributes msg ) :: Either AttributeError [MappedAddress])
      ext = fmap (fmap unMA) ma
  case ext of
       Right [ad] -> return (sock, ad)
       _ -> error "Couldn't STUN."
