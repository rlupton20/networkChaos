import Collector.PacketCapture

main :: IO ()
main = do
  putStrLn "Main thread..."
  "wlp3s0" `directTo` (\bs -> putStrLn $ show bs)
