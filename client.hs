import Network.Socket

main :: IO ()
main = withSocketsDo $ do
  listener <- socket AF_INET Stream defaultProtocol
  serverAddrs <- getAddrInfo Nothing (Just "localhost") (Just $ show 11111)
  let serverAddr = addrAddress $ serverAddrs!!0
  putStrLn $ show serverAddr
  bind listener serverAddr
  listen listener 4
  (conn, addrC) <- accept listener
  putStrLn $ show (conn, addrC)
  (fwd,_,_) <- recvFrom conn 4096
  putStrLn fwd
  
  (sock, addrS) <- makeConnectionToAddr "192.168.0.104" 12345
  sendTo sock fwd addrS -- not the best send code - use ByteString variant
  (fwd,_,_) <- recvFrom sock 4096
  sendTo conn fwd addrC
  close conn
  close listener
  close sock

makeConnectionToAddr :: String -> Int -> IO (Socket, SockAddr)
makeConnectionToAddr ip port = withSocketsDo $ do
  addrs <- getAddrInfo Nothing (Just ip) (Just $ show port)
  sock <- socket AF_INET Stream defaultProtocol
  let addr = addrAddress $ addrs!!0
  connect sock addr
  return (sock, addr)
