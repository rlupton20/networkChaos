module Command.CliTypes
( CliComm(..)
, CLI
, runCli ) where

import Command.Types (Command)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)

data CliComm = CliComm { sendCommand :: Command -> IO () }

type CLI = ReaderT CliComm IO

runCli :: CLI () -> CliComm -> IO ()
runCli cli clicomm = runReaderT cli $ clicomm
