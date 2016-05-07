{-# LANGUAGE BangPatterns #-}
module Types
( Addr
, addr
, addrW8
, renderAddr
, Packet
, Worker
, Injector ) where

import Utils
import Control.IO.Builder

import Data.Word
import qualified Data.ByteString as B

-- |Addr is a type for holding IP addresses. It is the same as the
-- type from network-house (note, the Show and Read instances are
-- different).
data Addr = Addr {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 deriving (Eq, Ord, Show, Read)

-- |renderAddr turns an Addr into a string in the way we would
-- normally expect. Mostly useful for debugging and testing.
renderAddr :: Addr -> String
renderAddr (Addr a b c d) = show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d

-- |addr takes a String and (tries) to convert it into an Addr
-- inside a Monad. Note that if the string contains Integers that
-- are bigger than 255, they are truncated, that is, the least
-- significant bits are taken. e.g.
-- addr "256.0.0.0" yields (Addr 0 0 0 0).
addr :: (Monad m) => String -> m Addr
addr str = do
  let ads = words $ map (\x -> if x == '.' then ' ' else x) str
  [a,b,c,d] <- sequence $ map readM ads
  let !ad = Addr a b c d
  return ad

-- |addrW8 takes four Word8s and uses them to build an Addr
-- e.g. addrW8 1 2 3 4 corresponds to 1.2.3.4
addrW8 :: Word8 -> Word8 -> Word8 -> Word8 -> Addr
addrW8 !a !b !c !d = Addr a b c d

type Packet = B.ByteString

-- |Injector is just a more descriptive name for the type which
-- wraps a ProcUnit which puts packets (ByteStrings) back into
-- a network device.
type Injector = Worker Packet
