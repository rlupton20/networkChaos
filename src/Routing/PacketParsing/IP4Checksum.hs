{-# LANGUAGE BangPatterns #-}
module Routing.PacketParsing.IP4Checksum
( ) where

import Data.List
import Data.Word
import Data.Bits
import qualified Data.ByteString as B

getHeader :: B.ByteString -> [Word8]
getHeader bs = take (ihl*4) bsu
               where
                 bsu = B.unpack bs
                 ihl = fromIntegral $ (bsu !! 0) .&. mask
                 !mask = foldl' (.|.) 0 $ map bit [0,1,2,3]
                 

to16 :: [Word8] -> [Word16]
to16 [] = []
to16 (xh:xl:t) = let !h = (xh16 `shift` 8) .|. (xl16) in h : to16 t
                 where
                   xh16 :: Word16
                   !xh16 = fromIntegral xh
                   xl16 :: Word16
                   !xl16 = fromIntegral xl
to16 _ = error "Bad header length"

extractChecksum :: [Word16] -> (Word16, [Word16])
extractChecksum pkt = (cs, rest)
                      where
                        cs = pkt !! 10
                        rest = h ++ t
                        h = take 9 pkt
                        t = drop 10 pkt

-- Assuming the checksum is missing from the header
-- calcChecksum computes what the checksum should be
--calcChecksum :: [Word16] -> Word16
--calcChecksum pkt = 
