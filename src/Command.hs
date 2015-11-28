module Command
( commandLine ) where

import Routing.RoutingTable

commandLine :: RoutingTable -> IO ()
commandLine rt = do
  cmd <- getLine
  if cmd == "quit" then return () else process rt cmd >> commandLine rt

process :: RoutingTable -> String -> IO ()
process _ cmd = putStrLn $ "Got: " ++ cmd
