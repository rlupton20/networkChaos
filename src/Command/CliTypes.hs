module Command.CliTypes where

import Command.Types
import Control.Monad.Trans.Reader

data CliComm = CliComm { sendCommand :: Command -> IO () }

type CLI = ReaderT CliComm IO

runCli :: CLI () -> CliComm -> IO ()
runCli cli clicomm = runReaderT cli $ clicomm
