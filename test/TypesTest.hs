module TypesTest where

import Test.HUnit

import Types

test :: Test
test = "addr doesn't convert a string" ~: (addr "String" :: Maybe Addr) @=? Nothing

test2 :: Test
test2 = "addr: addr and addrW8 agree" ~: (addr "1.2.34.123") @=? (Just $ addrW8 1 2 34 123)