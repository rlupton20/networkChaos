module Command.CommandLine
( commandLine ) where

import Command.CliTypes
import Command.Interaction
import Control.Monad.IO.Class (liftIO)

commandLine :: CLI ()
commandLine = do
  cmd <- liftIO $ getLine
  if cmd == "quit" then return () else process cmd >> commandLine

process :: String -> CLI ()
process cmd
  | cmd == "direct" = direct
  | otherwise = liftIO $ putStrLn $ "Invalid command: " ++ cmd
