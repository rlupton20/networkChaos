import Control.Concurrent

import Collector.PacketCapture
import Routing.Routing
import PacketParsing.Ether

main :: IO ()
main = do
  (routeChan, _) <- makeRouter
  etherStripper <- makeEtherStripper $ \bs -> bs `routeTo` routeChan
  "wlp3s0" `directTo` (\bs -> bs `routeTo` etherStripper)
  loop
  where
    loop = do
      loop
