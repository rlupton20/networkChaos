-- A basic TCP client - to develop for TLS connection

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Network.Socket.ByteString

main :: IO ()
main = withSocketsDo $ do
  -- Create a client socket
  client <- socket AF_INET Stream defaultProtocol

  -- Get information on the address. We can connect this client
  -- to the basic server we have already written.
  serverAddrs <- getAddrInfo Nothing (Just $ "localhost") (Just $ show 12345)
  let addr = addrAddress $ head serverAddrs

  -- Connect to the server and send a small message
  connect client addr
  sendTo client (pack "GET") addr

  -- Wait for a reply, and print it to screen
  (reply,_) <- recvFrom client 4096
  putStrLn $ show reply

  close client
