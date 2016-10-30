module Command.ControlTypes
( ControlEnvironment(..)
, Control
, takeControlOf
, withEnvironment ) where
 

import Network.Socket ( Socket )
import Command.Types (Command)
import Control.Concurrent.TreeThreads ( TreeThread, sproutOn, withEnvironment )

data ControlEnvironment = ControlEnvironment { controlSocket :: Socket
                                             , sendCommand :: Command -> IO () }

type Control = TreeThread ControlEnvironment

takeControlOf :: Control () -> ControlEnvironment -> IO ()
takeControlOf = sproutOn
