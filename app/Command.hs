module Command
( routeMaster ) where

import Control.Monad.IO.Class (liftIO)

import Manager

-- |commander is a Manager process which takes commands and
-- spawns submanagers to execute those commands.
routeMaster :: Manager ()
routeMaster = do
  cq <- withEnvironment commandQueue
  next <- liftIO $ getCommand cq
  case next of
    Quit -> return ()
    (Add a s) -> spawn (add a s) >> routeMaster
    (Remove a) -> spawn (remove a) >> routeMaster
