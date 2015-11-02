-- Basic TCP server - to develop for TLS connection
{-# LANGUAGE OverloadedStrings #-}

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import qualified Data.ByteString as B
import Data.String

import Network.TLS
import Control.Monad.IO.Class
import System.IO
import Data.X509
import Data.Default.Class
import Network.TLS.Extra.Cipher

main :: IO ()
main = withSocketsDo $ do
  -- Make a basic TCP socket
  server <- socket AF_INET Stream defaultProtocol
  -- Get information on the address we want to host at
  addrInfo <- getAddrInfo Nothing (Just "localhost") (Just $ show 12345)
  let addresses = map addrAddress addrInfo
  -- If we can, serve at this address
  if null addresses then error "Can't create server at specified address" else bind server (head addresses)
  listen server 5

  tlsParams <- makeServerParams
  putStrLn $ show tlsParams

  -- This loops forever, dispatching a basic message to anything that
  -- connects securely. One can connect with a web browser for instance,
  -- via https
  forever $ do
    -- Get the next connection, and show the details
    (connection, client) <- accept server
    putStrLn $ "Got connection from " ++ show client

    -- Enable encryption on the connection
    context <- contextNew connection tlsParams
    handshake context
    message <- recvData context
    putStrLn $ "Received: " ++ show message
    -- Send one back
    sendData context (fromString "Hello from secure Haskell!")
    bye context
    contextClose context
    
    close connection
  where forever a = a >> forever a

makeServerParams :: (MonadIO m) => m ServerParams
makeServerParams = do
  attempted <- liftIO $ credentialLoadX509 "cert.pem" "key.pem"
  -- Now pattern match credential, certificate chain and private key (ignored)
  let (Right cred@(cc, _)) = attempted
      CertificateChain listOfSignedCerts = cc
  return $ ServerParams False listOfSignedCerts Nothing (def {sharedCredentials = Credentials [cred]}) def (def {supportedCiphers = ciphersuite_all})
  
