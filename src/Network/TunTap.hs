{-# LANGUAGE ForeignFunctionInterface, PackageImports, RecordWildCards #-}
module Network.TunTap
( openTunTap
, TunTap(..)
, noPI
, TUNDevice
, openTUN
, closeTUN
, readTUN
, writeTUN ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import Data.List
import Data.Bits

import System.Posix.Types
import System.Posix.IO (closeFd)
import "unix-bytestring" System.Posix.IO.ByteString
import qualified Data.ByteString as B

data TunTap = TUN | TAP deriving (Show, Eq)
type TTFlag = CInt

noPI :: TTFlag
noPI = c_no_pi

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
openTunTap :: TunTap -> String -> [TTFlag] -> IO Fd
openTunTap tt name flags = do
  let init = if tt == TUN then c_tun else c_tap
      params = foldl' (.|.) init flags
  c_name <- newCString $ take (fromIntegral c_ifnamsiz) name

  fd <- c_getTunTap c_name params

  -- Note that c_getTunTap might return a bad file descriptor
  -- (in the case of an error)
  if fd < 0 then
    (error $ "Couldn't open TUN device: " ++ name)
    else
    return $ Fd fd

data TUNDevice = TUNDevice { name :: String
                           , fd :: Fd }

closeTUN :: TUNDevice -> IO ()
closeTUN TUNDevice{..} = closeFd fd

readTUN :: TUNDevice -> IO B.ByteString
readTUN TUNDevice{..} = fdRead fd 65536

writeTUN :: TUNDevice -> B.ByteString -> IO ()
writeTUN TUNDevice{..} bs = fdWrite fd bs >> return () 
  
openTUN :: String -> IO TUNDevice
openTUN name = do
    fd <- openTunTap TUN name [noPI]
    return $ TUNDevice name fd
