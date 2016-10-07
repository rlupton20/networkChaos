module Command.Control
( controller ) where

import Command.ControlTypes (Control)
import Control.Monad.IO.Class (liftIO)

controller :: Control ()
controller = do
  cmd <- liftIO $ getLine
  if cmd == "quit" then return () else process cmd >> controller

process :: String -> Control ()
process cmd
  | otherwise = liftIO $ putStrLn $ "Invalid command: " ++ cmd
