import Network.Pcap

interface :: String
interface = "rvl0" -- or use "any"
-- Ultimately the interface can be set to our virtual interface

main :: IO ()
main = do
  device <- openLive interface 0xFFFF False 0
  setFilter device "ether proto 0x0800" True 0
  
  loopBS device (-1) (\_ packet -> putStrLn $ show packet)
  return ()
