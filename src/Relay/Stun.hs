module Relay.Stun
( ) where

import Relay.Connection
import Network.Stun

stunServer = "stun.ekiga.net"

stun :: IO ()
stun = do
  brq <- bindRequest
  stunAddr <- resolveAddr stunServer 3478
  stunr <- findMappedAddress stunAddr 0 [] --stunRequest' stunAddr 0 [] brq
  putStrLn $ show stunr
