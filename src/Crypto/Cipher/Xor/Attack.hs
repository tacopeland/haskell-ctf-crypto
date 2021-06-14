module Crypto.Cipher.Xor.Attack where

import qualified Data.ByteString as B
import Data.List
import Data.Maybe
import Data.Ord

import Crypto.Helpers


    {-
       For finding the key length.
       Based on https://github.com/hellman/xortool
    -}

keyLengthCount :: B.ByteString -> Int -> Int
keyLengthCount ct i
  | i == 0 = 0
  | otherwise =
    let substrs [] = []
        substrs l = take i l : substrs (drop i l)
        ss = transpose (substrs (B.unpack ct))
        freqs = map freqTable ss
        maxes = map (snd . last . sortOn snd) freqs
        eqCount = sum (map (flip (-) 1) maxes)
     in fromInteger eqCount

calcFitnesses :: B.ByteString -> Int -> [(Int, Double)]
calcFitnesses ct maxlen =
    let lenCounts = map (\x -> (fromIntegral $ keyLengthCount ct x, fromIntegral x)) [1..maxlen]
        possible = map (\(x, i) -> (x / (fromIntegral maxlen + i ** 1.5), i)) lenCounts
        fitnesses = localMaxima (map fst possible)
        indices = map ((+ 1) . fromJust . flip elemIndex (map fst possible)) fitnesses
     in zip indices fitnesses

-- |Calculate the probability that a key is of length i, from i=1 to maxlen.
-- Then, take the top 10 keys of maximum probability.
calcProbabilities :: B.ByteString -> Int -> [(Int, Double)]
calcProbabilities ct maxlen =
    let fitnesses = calcFitnesses ct maxlen
        top10 = take 10 (sortOn (Down . snd) fitnesses)
        fitnessSum = (sum . map snd) top10
        normalized = map (\(i, x) -> (i, 100.0 * x / fitnessSum)) top10
     in normalized


    {-
        For guessing the key.
    -}


