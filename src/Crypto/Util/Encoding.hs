{-# LANGUAGE OverloadedStrings #-}
module Crypto.Util.Encoding where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Numeric (readHex, showHex)

-- |This can fail if not given a string of hex digits.
hexToInteger :: T.Text -> Integer
hexToInteger hex = fst (head (readHex (T.unpack hex)))

bytesToInteger :: B.ByteString -> Integer
bytesToInteger = B.foldl (\x y -> 256 * x + toInteger y) 0

textToInteger :: T.Text -> Integer
textToInteger = bytesToInteger . T.encodeUtf8

integerToHex :: Integer -> T.Text
integerToHex i = T.pack (showHex i "")

integerToBytes :: Integer -> B.ByteString
integerToBytes n =
    let inner n
          | n == 0 = []
          | otherwise = inner (n `div` 256) ++ [chr (fromInteger (n `mod` 256))]
     in C.pack (inner n)

integerToText = T.decodeUtf8 . integerToBytes
