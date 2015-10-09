import Network.Socket
import Data.Word

-- This works but gives all TCP packets to localhost (i.e. port ignored)
-- Also Raw sockets don't pick up all IP traffic, just a particular protocol (e.g. TCP);
-- one needs to use a packet socket for any protocol.

main :: IO ()
main = withSocketsDo $ do
  listener <- socket AF_INET Raw 6
  serverAddrs <- getAddrInfo Nothing (Just "localhost") (Just $ show 12345)
  let serverAddr = addrAddress $ serverAddrs!!0
  putStrLn $ show serverAddr

  bind listener serverAddr
  str <- (recv listener 4096) 
  putStrLn str
  close listener
