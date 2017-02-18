module Command
( routeMaster ) where

import Control.Monad.IO.Class (liftIO)
import Network.Socket ( Socket, PortNumber, SockAddr
                      , AddrInfo(..), getAddrInfo, close)
import Network.Socket.ByteString (sendTo, recv)
import Control.Exception (bracket_)
import Control.Monad (forever)
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, putTMVar, takeTMVar)

import Routing.RoutingTable (withRoute, (#->), getInjector)

import Command.Types (Connection(..), errorBracketedPending)

import Manager
import Core

-- |commander is a Manager process which takes commands and
-- spawns submanagers to execute those commands.
routeMaster :: Manager ()
routeMaster = do
  cq <- withEnvironment commandQueue
  next <- liftIO $ getCommand cq
  case next of
    Quit -> return ()
    (Create uid cb) -> spawn (new uid cb) >> routeMaster
    (Direct l r pb) -> spawn (direct l r pb) >> routeMaster
    (Remove a) -> spawn (remove a) >> routeMaster

direct :: Addr -> (Addr, PortNumber) -> TMVar Connection -> Manager ()
direct l (r,p) pb = do
  env <- environment
  liftIO $ withProtectedBoundUDPSocket $ \sock -> do
    q <- newQueue
    withRoute (routingTable env) (l #-> (r,q)) $ do
      atomically . putTMVar pb $ Connection l r (fromIntegral p)
      makeRelay sock (getInjector . routingTable $ env) q (r, fromIntegral p)

makeRelay :: Socket -> Injector -> PacketQueue -> (Addr, PortNumber) -> IO ()
makeRelay s inj q (a,p) = do
  let dest = buildAddress a p
  race_ (dispatch q dest) (inject s)
  where
    dispatch q dest = forever $ do
      bs <- readQueue q
      sendTo s bs dest

    inject s = forever $ do
      bs <- recv s 4096
      bs `passTo` inj

new :: Int -> TMVar (Addr, PortNumber) -> Manager ()
new uid cb = do
  env <- environment
  let p = pending env
  liftIO $ withProtectedBoundUDPSocket $ \sock -> do
    (Connection v a p) <- errorBracketedPending p uid $ \tc -> do
      c <- describeSocket sock
      atomically $ putTMVar cb c
      atomically $ takeTMVar tc
    q <- newQueue
    withRoute (routingTable env) (v #-> (a,q)) $
      makeRelay sock (getInjector . routingTable $ env) q (a, fromIntegral p)

remove :: Addr -> Manager ()
remove _ = liftIO $ putStrLn "remove"
