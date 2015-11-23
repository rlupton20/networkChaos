import Control.Concurrent

import Collector.PacketCapture
import Routing.Routing

main :: IO ()
main = do
  (routeChan, _) <- makeRouter
  "wlp3s0" `directTo` (\bs -> bs `routeTo` routeChan)
  loop
  where
    loop = do
      loop
