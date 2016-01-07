module Utils
( readM
, bracketForkFinally
, bracketFork
, rethrowException ) where

import Text.Read

import Control.Exception
import Control.Concurrent

readM :: (Read a, Monad m) => String -> m a
readM str = do
  let d = readMaybe str
  case d of
    Nothing -> fail $ "Could not read string: " ++ str
    Just parse -> return parse

bracketForkFinally :: IO a -> (a -> (Either SomeException c) -> IO ()) -> (a -> IO c) -> IO ThreadId
bracketForkFinally acquire end action = do
  mask $ \restore -> do
    res <- acquire
    forkFinally (restore $ action res) (end res)

bracketFork :: IO a -> (a -> IO ()) -> (a -> IO c) -> IO ThreadId
bracketFork acquire release action =
  bracketForkFinally acquire (\res _ -> release res) action

rethrowException :: (Exception e) => Either e a -> IO ()
rethrowException val = case val of
  Left e -> throwIO e
  Right _ -> return ()
