{-# LANGUAGE BangPatterns #-}
module Utils
( readM
, passWork ) where

import Text.Read

import Control.IO.Builder (passWork)

import Data.Word

import Control.Exception
--import Control.Concurrent
--import Control.Concurrent.Async

readM :: (Read a, Monad m) => String -> m a
readM str = do
  let d = readMaybe str
  case d of
    Nothing -> fail $ "Could not read string: " ++ str
    Just parse -> return parse
