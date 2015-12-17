{-# LANGUAGE ForeignFunctionInterface #-}
module Collector.Posix.TunTap
( openTunTap
, TunTap(..) ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import Data.List
import Data.Bits

import System.IO
import System.Posix.Types
import System.Posix.IO

data TunTap = TUN | TAP deriving (Show, Eq)
type TTFlag = CInt

foreign import ccall "tuntap.h tun"
  c_tun :: CInt

foreign import ccall "tuntap.h tap"
  c_tap :: CInt

foreign import ccall "tuntap.h no_pi"
  c_no_pi :: CInt

foreign import ccall "tuntap.h ifnamsiz"
  c_ifnamsiz :: CInt

foreign import ccall "tuntap.h getTunTap"
  c_getTunTap :: Ptr CChar -> CInt -> IO CInt


-- Convert our C function into a Haskell function
openTunTap :: TunTap -> String -> [TTFlag] -> IO Handle
openTunTap tt name flags = do
  let init = if tt == TUN then c_tun else c_tap
      params = foldl' (.|.) init flags
  c_name <- newCString $ take (fromIntegral c_ifnamsiz) name

  fd <- c_getTunTap c_name params
  
  handle <- fdToHandle $ Fd fd
  return handle
