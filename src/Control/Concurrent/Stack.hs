{-# LANGUAGE RecordWildCards #-}
module Control.Concurrent.Stack
( Stack
, register
, runStack
, blocksInForeign ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class

type ProcStack = [IO ()]
type Stack = StateT ProcStack IO

addToStack :: IO () -> ProcStack -> ProcStack
addToStack io ios = (io:ios)

register :: IO () -> Stack ()
register io = modify' (addToStack io)

buildStack :: Stack a -> IO ProcStack
buildStack ls = execStateT ls []

runStack :: Stack a -> IO ()
runStack stck = do
  ios <- buildStack stck
  rem <- launchStack ios []
  -- Now we need to wait for all the exception handlers to run.
  -- Note we don't want to rethrow exceptions (one exception in
  -- Stack means shut down everything), so we just use waitCatch
  sequence $ map waitCatch rem
  return ()
  where
    launchStack [] ws = waitAny ws >> return ws
    launchStack (s:ss) ws = withAsync s $ \w -> launchStack ss (w:ws)

blocksInForeign :: IO a -> IO a
blocksInForeign io = do
  a <- async io
  wait a

-- TEST AREA

testStack :: Stack ()
testStack = do
  register (forever $ threadDelay 1000000 >> putStrLn "T1")
  register (threadDelay 10000000)
  register (forever $ threadDelay 2000000 >> putStrLn "T2")

test :: IO ()
test = runStack testStack