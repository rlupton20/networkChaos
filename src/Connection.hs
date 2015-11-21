{-# LANGUAGE OverloadedStrings #-}
module Connection where

import Data.ByteString.Char8 as B
import Data.String

import System.IO

class Connection a where
  receive :: a -> IO ByteString
  send :: a -> ByteString -> IO ()

-- StdIO is a debugging connection, which simply makes use of the
-- standard input and output
data StdIO = StdIO

instance Connection StdIO where
  receive a = B.getLine
  send a str = B.putStrLn $ str
