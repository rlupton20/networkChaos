module Command.ControlTypes
( ControlEnvironment(..)
, Control
, runControl ) where

import Command.Types (Command)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)

data ControlEnvironment = ControlEnvironment { sendCommand :: Command -> IO () }

type Control = ReaderT ControlEnvironment IO

runControl :: Control () -> ControlEnvironment -> IO ()
runControl ctl controlenv = runReaderT ctl $ controlenv
