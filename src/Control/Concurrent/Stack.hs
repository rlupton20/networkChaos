{-# LANGUAGE RecordWildCards, ExistentialQuantification, RankNTypes #-}
module Control.Concurrent.Stack
( Stack
, register
, runStack
, blocksInForeign ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception

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
  reg <- newTVarIO []

  -- Now we run the stack, ensuring that if the Stack
  -- finishes, then all exception handlers are allowed to
  -- run.
  launchStack reg ios [] `finally`
    ( do as <- readTVarIO reg; sequence $ map waitCatch as )    
  return ()
  where
    launchStack _ [] ws = waitAny ws
    launchStack reg (s:ss) ws = withRegisteredAsync reg s $ \w ->
      launchStack reg ss (w:ws)

blocksInForeign :: IO a -> IO a
blocksInForeign io = do
  a <- async io
  wait a

-- We need a modified withAsync functions, since we
-- would like to wait for all exception handlers to run before
-- quitting. Since we may lose track of an Async if an exception strikes
-- between spawning and registering it (in e.g. a TVar),
-- we mask the registration.
-- The running Asyncs are then visible in a TVar from the exception
-- handler, so it is possible to wait for them to finish outside of
-- withAsync.

-- |withRegisteredAsync is like with withAsync, but the Async is added to
-- the list stored in the passed TVar. Interruptions are masked while
-- the Async is added to the list.
withRegisteredAsync :: TVar [Async a] -> IO a -> (Async a -> IO b) -> IO b
withRegisteredAsync reg io inner = do
  withAsyncMaskInner io $ \restore a -> do
    atomically $ modifyTVar' reg (\as -> a:as)
    restore $ inner a

-- |withAsyncMaskInner is like withAsync, but the inner computation starts
-- masked, and is given a restore function.
withAsyncMaskInner :: IO a -> ((forall a . IO a -> IO a) -> Async a -> IO b) -> IO b
withAsyncMaskInner io inner = do
  mask $ \restore -> do
    withAsync (restore io) $ \a -> inner restore a

-- TEST AREA

testStack :: Stack ()
testStack = do
  register (forever $ threadDelay 1000000 >> putStrLn "T1")
  register (threadDelay 10000000)
  register (forever $ threadDelay 2000000 >> putStrLn "T2")

test :: IO ()
test = runStack testStack
