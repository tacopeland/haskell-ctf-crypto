module CTF.Prelude
    ( b64Enc, b64Dec
    , hexToInteger, integerToHex
    , bytesToInteger, integerToBytes
    , textToInteger, integerToText
    , hexToBytes, hexToText
    , chr, ord
    , Text(..), ByteString(..)
    , tPack, bPack, tUnpack, bUnpack
    , rot, xor
    , xgcd, divides, modInv, modMul, modPow
    , module NumberTheory.Integers
    , module NumberTheory.Factor
    , module NumberTheory.Primes
    ) where

import Crypto.Cipher.AES (AES256)
import Data.Char (chr, ord)
import Data.ByteString.Base64 (encodeBase64, decodeBase64Lenient)
import Data.ByteString (ByteString(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Text (Text(..))
import qualified Data.Text as T

import CTF.Crypto.Cipher.Monoalphabetic (rot)
import CTF.Crypto.Cipher.Xor (bytesXor)
import CTF.Crypto.Util.Encoding
  ( hexToInteger, integerToHex
  , bytesToInteger, integerLEToBytes, integerBEToBytes
  , textToInteger, integerLEToText, integerBEToText
  , hexToBytes, hexToText
  )
import NumberTheory.Basic (xgcd, divides)
import NumberTheory.Integers
import NumberTheory.Factor
import NumberTheory.Modular (mInv, modInv, modMul, modPow)
import NumberTheory.Primes

b64Enc :: ByteString -> Text
b64Enc = encodeBase64

b64Dec :: ByteString -> ByteString
b64Dec = decodeBase64Lenient

tPack = T.pack
bPack = BSC.pack

tUnpack = T.unpack
bUnpack = BS.unpack

-- When in doubt, use little-endian
integerToBytes = integerLEToBytes
integerToText = integerLEToText

xor = bytesXor

{-
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

integerToText :: Integer -> T.Text
integerToText = T.decodeUtf8 . integerToBytes

hexToBytes :: T.Text -> B.ByteString
hexToBytes = integerToBytes . hexToInteger

hexToText :: T.Text -> T.Text
-}
