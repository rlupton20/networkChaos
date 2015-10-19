-- Basic TCP server - to develop for TLS connection

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Network.Socket.ByteString

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
  
  -- This loops forever, dispatching a basic message to anything that
  -- connectes. One can connect with a web browser for instance.
  forever $ do
    -- Get the next connection, and show the details
    (connection, client) <- accept server
    putStrLn $ "Got connection from " ++ show client
    -- Receive a message
    (message,_) <- recvFrom connection 4096
    putStrLn $ "Received: " ++ show message
    -- Send one back
    sendTo connection (pack "Hello from Haskell!") client
    close connection
  where forever a = a >> forever a
