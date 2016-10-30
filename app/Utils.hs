module Utils
( readM
, withUnixSocket
, withControlSocket ) where

import Text.Read (readMaybe)
import Control.Exception ( bracket, bracket_ )
import Network.Socket ( Family(AF_UNIX), SocketType(Stream), SockAddr(SockAddrUnix)
                      , Socket, defaultProtocol, socket, bind, close )
import System.Posix.Files ( removeLink )

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
