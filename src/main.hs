import Control.Concurrent

import Collector.PacketCapture
import Routing

main :: IO ()
main = do
  putStrLn "Main thread..."

  routeChan <- makeRouter
  "wlp3s0" `directTo` (\bs -> bs `routeTo` routeChan)
  loop
  where
    loop = do
      loop
