{-# LANGUAGE ForeignFunctionInterface, PackageImports, RecordWildCards #-}
module Network.TunTap
( openTunTap
, TTType(..)
, noPI
, TunTap
, openTUN
, closeTT
, readTT
, writeTT ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Array

import Data.List
import Data.Bits

import System.Posix.Types
import System.Posix.IO (closeFd)
import "unix-bytestring" System.Posix.IO.ByteString
import qualified Data.ByteString as B

data TTType = TUN | TAP deriving (Show, Eq)
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
openTunTap :: TTType -> String -> [TTFlag] -> IO (Fd, String)
openTunTap tt name flags = allocaArray0 (fromIntegral c_ifnamsiz) $ \cstr -> do
  let init = if tt == TUN then c_tun else c_tap
      params = foldl' (.|.) init flags
      cnul = castCharToCChar '\0'
      ccname = fmap (castCharToCChar) $ take (fromIntegral c_ifnamsiz) name

  (fd, asname) <- withArray ccname $ \cname -> do
                                     lname <- lengthArray0 cnul cname
                                     copyArray cstr cname (lname+1)
                                     fd <- c_getTunTap cstr params
                                     asname <- peekCString cstr
                                     return (fd, asname)

  putStrLn $ "Opened device " ++ asname

  -- Note that c_getTunTap might return a bad file descriptor
  -- (in the case of an error)
  if fd < 0 then
    (error $ "Couldn't open TUN device: " ++ name)
    else
    return $ (Fd fd, asname)

data TunTap = TunTap { name :: String
                     , tttype :: TTType
                     , fd :: Fd }

closeTT :: TunTap -> IO ()
closeTT TunTap{..} = closeFd fd

readTT :: TunTap -> IO B.ByteString
readTT TunTap{..} = fdRead fd 65536

writeTT :: TunTap -> B.ByteString -> IO ()
writeTT TunTap{..} bs = fdWrite fd bs >> return () 
  
openTUN :: String -> IO TunTap
openTUN name = do
    (fd, asname) <- openTunTap TUN name [noPI]
    return $ TunTap asname TUN fd
