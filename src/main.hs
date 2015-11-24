import Control.Concurrent

import Collector.PacketCapture
import Routing.Routing
import Routing.RoutingTable
import Routing.PacketParsing.Ether

-- Only needed when setting up router
import Routing.PacketParsing.IP4 (addr)
import Control.Concurrent.STM.TQueue
import qualified Data.ByteString as B
-- End temporary section

main :: IO ()
main = do
  (routeChan, rt) <- makeRouter
  createRoutingTable rt
  etherStripper <- makeEtherStripper $ \bs -> bs `routeTo` routeChan
  "wlp3s0" `directTo` (\bs -> bs `routeTo` etherStripper)
  loop
  where
    loop = do
      loop

-- Test function, building a basic routing table
createRoutingTable :: RoutingTable -> IO ()
createRoutingTable rt = do
  rt `setLocal` (addr "192.168.1.100")
  rt `setVirtual` (addr "10.0.0.1")
  q <- newTQueueIO
  newRoute rt (addr "10.0.0.0") (addr "10.0.0.10", q)
  return ()
