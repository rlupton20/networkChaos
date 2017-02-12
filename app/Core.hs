{-# LANGUAGE BangPatterns, DeriveGeneric #-}
module Core
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
, readQueue
, readM
, withUnixSocket
, withControlSocket
, withProtectedUDPSocket
, withProtectedBoundUDPSocket
, describeSocket ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO
                                     , readTQueue, writeTQueue)
import qualified Data.ByteString as B
import Data.Word (Word8)

import Text.Read (readMaybe)
import Control.Exception ( bracket, bracket_, bracketOnError )
import Network.Socket ( Family(AF_UNIX, AF_INET)
                      , SocketType(Stream, Datagram)
                      , SockAddr(SockAddrUnix, SockAddrInet)
                      , Socket, PortNumber
                      , iNADDR_ANY, aNY_PORT
                      , defaultProtocol, socket, bind, close
                      , getSocketName, hostAddressToTuple )
import System.Posix.Files ( removeLink )
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

-- |Addr is a type for holding IP addresses. It is the same as the
-- type from network-house (note, the Show and Read instances are
-- different).
data Addr = Addr {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON Addr
instance ToJSON Addr

-- |addrW8 takes four Word8s and uses them to build an Addr
-- e.g. addrW8 1 2 3 4 is the address 1.2.3.4
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

-- Internally threads have queues of packets which we must process.
-- Here we abstract away the underlying types and provide interface
-- functions for creating, reading and writing to these queues.

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


readM :: (Read a, Monad m) => String -> m a
readM str = do
  let d = readMaybe str
  case d of
    Nothing -> fail $ "Could not read string: " ++ str
    Just parse -> return parse


-- |withUnixSocket opens a new (streaming) unix socket, with the promise
-- that it will be closed in the event of an exception, or when the passed
-- action is finished.
withUnixSocket :: (Socket -> IO a) -> IO a
withUnixSocket = bracket
  (socket AF_UNIX Stream defaultProtocol)
  close

-- |withControlSocket opens a (streaming) unix socket which is bound to
-- a passed path. It promises to remove the binding, and clean up the socket
-- in the event of an exception, or after the action has completed running.
withControlSocket :: String -> (Socket -> IO a) -> IO a
withControlSocket path action = withUnixSocket $ \sock ->
    bracket_ (bind sock $ SockAddrUnix path)
             (removeLink path)
             (action sock)

-- |withUDPSocket opens a new UDP socket, with the promise that it will be
-- closed in the event of an exception during the execution of the passed
-- action. One the socket leaves the execution of the action, it is no longer
-- guaranteed to be closed.
withProtectedUDPSocket :: (Socket -> IO a) -> IO a
withProtectedUDPSocket = bracketOnError
  (socket AF_INET Datagram defaultProtocol)
  close

-- |withBoundUDPSocket opens a new UDP socket and binds it to an address
withProtectedBoundUDPSocket :: (Socket -> IO a) -> IO a
withProtectedBoundUDPSocket action = withProtectedUDPSocket $ \sock ->
  bracketOnError (bind sock $ SockAddrInet aNY_PORT iNADDR_ANY)
                 (const $ close sock)
                 (const $ action sock)

describeSocket :: Socket -> IO (Maybe (Addr, PortNumber))
describeSocket sock = do
  (SockAddrInet p ha) <- getSocketName sock
  let (a, b, c, d) = hostAddressToTuple ha
  return $ Just (Addr a b c d, p)
