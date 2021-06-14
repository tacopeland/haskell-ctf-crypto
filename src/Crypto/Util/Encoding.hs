{-# LANGUAGE OverloadedStrings #-}
module Crypto.Util.Encoding where

import Data.ByteString as B
import Data.ByteString.Char8 as C
import Data.Char

bytesToInteger :: ByteString -> Integer
bytesToInteger = B.foldl (\x y -> 256 * x + toInteger y) 0

strToInteger :: String -> Integer
strToInteger = bytesToInteger . C.pack

integerToBytes :: Integer -> ByteString
integerToBytes n =
    let inner n
          | n == 0 = []
          | otherwise = inner (n `div` 256) ++ [chr (fromInteger (n `mod` 256))]
     in C.pack (inner n)
