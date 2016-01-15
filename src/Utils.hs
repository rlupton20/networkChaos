{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Utils
( readM
, passWork
, withAsyncMaskInner ) where

import Text.Read

import Control.IO.Builder (passWork)

import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async

readM :: (Read a, Monad m) => String -> m a
readM str = do
  let d = readMaybe str
  case d of
    Nothing -> fail $ "Could not read string: " ++ str
    Just parse -> return parse

withAsyncMaskInner :: IO a -> ((forall a . IO a -> IO a) -> Async a -> IO b) -> IO b
withAsyncMaskInner io inner = do
  mask $ \restore -> do
    withAsync (restore io) $ \a -> inner restore a
