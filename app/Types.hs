{-# LANGUAGE BangPatterns #-}
module Types
( Addr
, addrW8
, renderAddr
, addr
, Queue
, Packet
, PacketQueue
, Injector
, passTo
, newQueue
, readQueue) where

import Utils (readM)

import Data.Word
import qualified Data.ByteString as B
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

-- |Addr is a type for holding IP addresses. It is the same as the
-- type from network-house (note, the Show and Read instances are
-- different).
data Addr = Addr {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 deriving (Eq, Ord, Show, Read)

-- |addrW8 takes four Word8s and uses them to build an Addr
-- e.g. addrW8 1 2 3 4 corresponds to 1.2.3.4
addrW8 :: Word8 -> Word8 -> Word8 -> Word8 -> Addr
addrW8 !a !b !c !d = Addr a b c d

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


type Queue a = TQueue a
type Packet = B.ByteString
type PacketQueue = Queue Packet
type Injector = PacketQueue

passTo :: a -> Queue a -> IO ()
passTo x q = atomically $ writeTQueue q x

newQueue :: IO (Queue a)
newQueue = newTQueueIO

readQueue :: Queue a -> IO a
readQueue = atomically . readTQueue
