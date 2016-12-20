module Command.ControlTypes
( ControlEnvironment(..)
, Control
, takeControlOf
, forkWith
, environment
, withEnvironment
, local ) where
 

import Network.Socket ( Socket )
import Manager (Command)
import Control.Concurrent.TreeThreads ( TreeThread, Branch
                                      , sproutOn, sprout
                                      , environment, withEnvironment, local )

data ControlEnvironment = ControlEnvironment { controlSocket :: Socket
                                             , sendCommand :: Command -> IO () }

type Control = TreeThread ControlEnvironment

takeControlOf :: Control () -> ControlEnvironment -> IO ()
takeControlOf = sproutOn

forkWith :: Control () -> Control Branch
forkWith = sprout
