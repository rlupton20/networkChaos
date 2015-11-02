-- A basic TCP client - to develop for TLS connection
{-# LANGUAGE OverloadedStrings #-}

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import qualified Data.ByteString as B
import Control.Monad.IO.Class

import Network.TLS
import Network.TLS.Extra.Cipher
import Data.String
import Data.X509
import Data.X509.CertificateStore
import Data.X509.File

main :: IO ()
main = withSocketsDo $ do
  -- Create a client socket
  client <- socket AF_INET Stream defaultProtocol

  -- Get information on the address. We can connect this client
  -- to the basic server we have already written.
  serverAddrs <- getAddrInfo Nothing (Just $ "localhost") (Just $ show 12345)
  let addr = addrAddress $ head serverAddrs

  -- Connect to the server and send a small encrypted message
  connect client addr

  -- Using self-signed certificates requires clients to know
  -- such certificates are trusted. We load them and make our
  -- client aware
  caCerts <- loadCACerts

  -- This should be possible to tidy up a bit (lenses?)
  let tlsParams' = defaultParamsClient "localhost" B.empty
      sup = clientSupported tlsParams'
      newSupported = sup { supportedCiphers = ciphersuite_all }
      shar = clientShared tlsParams'
      newShared = shar { sharedCAStore = caCerts }
      tlsParams = tlsParams' { clientSupported = newSupported, clientShared = newShared, clientUseServerNameIndication = False }

  -- Establish encrypted connection
  context <- contextNew client tlsParams
  handshake context
  -- Send an encrypted message
  sendData context (fromString "GET")

  -- Receive an encrypted message
  reply <- recvData context
  putStrLn $ show reply

  close client

loadCACerts :: (MonadIO m) => m CertificateStore
loadCACerts = liftIO $ do
  signedCerts <- readSignedObject "cert.pem"
  let certStore = makeCertificateStore signedCerts
  return certStore
