module Crypto.Cipher.Xor where

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

bytesXor :: B.ByteString -> B.ByteString -> B.ByteString
bytesXor a b =
    let lena = B.length a
        lenb = B.length b
        maxLen = max lena lenb
        (shorter, longer) = if lena < lenb then (a, b) else (b, a)
        list = B.unpack shorter
        normalized = B.pack (take maxLen (cycle list))
     in B.pack (B.zipWith xor normalized longer)

strXor :: String -> String -> String
strXor a b = C.unpack (bytesXor (C.pack a) (C.pack b))
