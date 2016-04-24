module Command.Types where

import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TQueue

-- |CommandQueue is an abstraction of the programs internal list of
-- pending commands.
data CommandQueue = CommandQueue (TQueue Command)

-- |getCommand is a utility wrapper for fetching the next command
getCommand :: CommandQueue -> IO Command
getCommand cq = let (CommandQueue q) = cq in atomically $ readTQueue q

-- |postCommand is a utility wrapper for posting a command
postCommand :: CommandQueue -> Command -> IO ()
postCommand cq c = let (CommandQueue q) = cq in atomically $ writeTQueue q c

-- |newCommandQueue provides us with a new CommandQueue
newCommandQueue :: IO CommandQueue
newCommandQueue = do
  q <- newTQueueIO
  return (CommandQueue q)

-- |The Command data type contains constructors for data used with different
-- types of command. Each command should have a protocol for how each piece of
-- data is provided. This provides a seperation between internal execution of
-- commands, and how the commands are delivered by the user. The user interfaces
-- simply have to make use of the correct protocol,
data Command = Quit | DirectConnection (TMVar Int) (TMVar String) (TMVar String) (TMVar String) deriving (Eq)
