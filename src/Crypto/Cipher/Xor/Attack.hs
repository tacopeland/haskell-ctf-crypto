module Crypto.Cipher.Xor.Attack where

import Helpers
import Crypto.Cipher.Xor

import Control.Parallel.Strategies
import qualified Data.ByteString as B
import Data.Char (isPrint, chr, ord)
import Data.List
import Data.Maybe
import Data.Ord
import Data.Word


asciiPrintable :: [Word8]
asciiPrintable = map (fromIntegral . ord)
    "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~ \t\n\r\x0b\x0c"


    {-
       For finding the key length.
       Based on https://github.com/hellman/xortool
    -}

keyLengthCount :: B.ByteString -> Integer -> Integer
keyLengthCount ct i
  | i == 0 = 0
  | otherwise =
    let substrs [] = []
        substrs l = take (fromIntegral i) l : substrs (drop (fromIntegral i) l)
        ss = transpose (substrs (B.unpack ct))
        freqs = map freqTable ss
        maxes = map (snd . last . sortOn snd) freqs
        eqCount = sum (map (flip (-) 1) maxes)
     in eqCount

calcFitnesses :: B.ByteString -> Integer -> [(Integer, Double)]
calcFitnesses ct maxLen =
    let lenCounts = map (\x -> (fromIntegral $ keyLengthCount ct x, fromIntegral x)) [1..maxLen]
        possible = map (\(x, i) -> (x / (fromIntegral maxLen + i ** 1.5), i)) lenCounts
        fitnesses = localMaxima (map fst possible)
        indices = map (toInteger . (+ 1) . fromJust . flip elemIndex (map fst possible)) fitnesses
     in zip indices fitnesses

-- |Calculate the probability that a key is of length i, from i=1 to maxLen.
-- Then, take the top 10 keys of maximum probability.
calcProbabilities :: B.ByteString -> Integer -> [(Integer, Double)]
calcProbabilities ct maxLen =
    let fitnesses = calcFitnesses ct maxLen
        top10 = take 10 (sortOn (Down . snd) fitnesses)
        fitnessSum = (sum . map snd) top10
        normalized = map (\(i, x) -> (i, 100.0 * x / fitnessSum)) top10
     in normalized


guessKeyLength :: B.ByteString -> Integer -> Integer
guessKeyLength ct maxLen = fst $ head $ calcProbabilities ct maxLen


    {-
        For guessing the key.
    -}

probableChar :: B.ByteString -> B.ByteString -> [Word8] -> Bool
probableChar ct char charset = B.all (`elem` charset) (bytesXor ct char)

probableChars :: B.ByteString -> [Word8] -> [Word8]
probableChars ct charset =
    filter (\char -> probableChar ct (B.pack [char]) charset) (enumFrom 0 :: [Word8])

probableKeys :: B.ByteString -> [Word8] -> Integer -> [B.ByteString]
probableKeys ct charset keyLen =
    let substrs :: B.ByteString -> [B.ByteString]
        substrs l
          | B.length l == 0 = []
          | otherwise = B.take (fromIntegral keyLen) l
                            : substrs (B.drop (fromIntegral keyLen) l)
        ss = B.transpose (substrs ct)
        possible = map (`probableChars` charset) ss
     in map B.pack (sequence possible)

attackKnownPlaintext :: B.ByteString -> [Word8] -> Integer -> B.ByteString
                      -> [(B.ByteString, B.ByteString)]
attackKnownPlaintext ct charset keyLen pt =
    parMap rpar (\key -> (key, bytesXor key ct))
        (filter (\key -> B.isInfixOf pt (bytesXor key ct))
            (probableKeys ct charset keyLen))


