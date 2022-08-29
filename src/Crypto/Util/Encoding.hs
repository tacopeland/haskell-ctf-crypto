{-# LANGUAGE OverloadedStrings #-}

{-| This packs integers in little-endian form by default, but offers
   big-endian forms of the integer packing function.
-}
module Crypto.Util.Encoding where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Numeric (readHex, showHex)

-- |This can fail if not given a string of hex digits. Make this return maybe.
hexToInteger :: T.Text -> Integer
hexToInteger hex = fst (head (readHex (T.unpack hex)))

-- Little-endian
bytesToInteger :: B.ByteString -> Integer
bytesToInteger = B.foldl (\x y -> 256 * x + toInteger y) 0

textToInteger :: T.Text -> Integer
textToInteger = bytesToInteger . T.encodeUtf8

integerToHex :: Integer -> T.Text
integerToHex i = if odd (T.length res) then T.cons '0' res else res
    where res = T.pack (showHex i "")

integerLEToBytes :: Integer -> B.ByteString
integerLEToBytes n =
    let inner n
          | n == 0 = []
          | otherwise = inner (n `div` 256) ++ [chr (fromInteger (n `mod` 256))]
     in C.pack (inner n)

integerBEToBytes :: Integer -> B.ByteString
integerBEToBytes n =
    let inner n
          | n == 0 = []
          | otherwise = (chr (fromInteger (n `mod` 256))) : (inner (n `div` 256))
     in C.pack (inner n)

integerLEToText :: Integer -> T.Text
integerLEToText = T.decodeUtf8 . integerLEToBytes

integerBEToText :: Integer -> T.Text
integerBEToText = T.decodeUtf8 . integerBEToBytes


-- |This packs the integer in little-endian form.
hexToBytes :: T.Text -> B.ByteString
hexToBytes = integerLEToBytes . hexToInteger

bytesToHex :: B.ByteString -> T.Text
bytesToHex = integerToHex . bytesToInteger

-- |This packs the integer in little-endian form.
hexToText :: T.Text -> T.Text
hexToText = T.decodeUtf8 . hexToBytes

