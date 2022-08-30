module CTF.Crypto.Cipher.Misc where

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.List as L

repeatedBlocks :: B.ByteString -> Int -> Int
repeatedBlocks bytes n = length blocks - length (L.nub blocks)
    where split bytes
            | B.length bytes == 0 = [bytes]
            | otherwise = B.take n bytes : split (B.drop n bytes)
          blocks = split bytes
