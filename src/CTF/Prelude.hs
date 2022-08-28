module CTF.Prelude
    ( b64Enc, b64Dec
    , hexToInteger, integerToHex
    , bytesToInteger, integerToBytes
    , textToInteger, integerToText
    , hexToBytes, hexToText
    , chr
    , Text(..), ByteString(..)
    , tPack, bPack, tUnpack, bUnpack
    ) where

import Data.Char (chr)
import Data.ByteString.Base64 (encodeBase64, decodeBase64Lenient)
import Data.ByteString (ByteString(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Text (Text(..))
import qualified Data.Text as T

import Crypto.Util.Encoding
  ( hexToInteger, integerToHex
  , bytesToInteger, integerLEToBytes, integerBEToBytes
  , textToInteger, integerLEToText, integerBEToText
  , hexToBytes, hexToText
  )


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
