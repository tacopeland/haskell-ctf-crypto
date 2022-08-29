module Crypto.Cipher.Xor where

import Helpers

import Control.Parallel.Strategies
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (isPrint, chr, ord)
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.Ord
import Data.Word

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

validThreshold :: Float
validThreshold = 0.95

-- If the result of xor(ct, char) meets the validity threshold, this is true.
possibleChar :: B.ByteString -> Word8 -> [Word8] -> Bool
possibleChar ct char charset = percentage > validThreshold
    where filtered = B.filter (`elem` charset) (bytesXor ct (B.pack [char]))
          percentage = fromIntegral (B.length filtered) / fromIntegral (B.length ct)

-- Get all single-byte xor keys such that the result is in the right charset.
possibleChars :: B.ByteString -> [Word8] -> [Word8]
possibleChars ct charset =
    filter (\char -> possibleChar ct char charset) (enumFrom 0 :: [Word8])


offsetStrs :: B.ByteString -> Int -> [B.ByteString]
offsetStrs ct len = B.transpose (substrs ct)
  where substrs l
          | B.length l == 0 = []
          | otherwise = B.take len l
                            : substrs (B.drop len l)

countBytes :: B.ByteString -> M.Map Word8 Integer
countBytes = B.foldl (\countMap byte ->
    M.alter (\jValue ->
        case jValue of
          Just value -> Just (value + 1)
          Nothing -> Just 1
          ) byte countMap) M.empty

mostCommonBytes :: B.ByteString -> [Word8]
mostCommonBytes bs =
    let count = M.toList (countBytes bs)
        maxFreq = maximum (map snd count)
     in map fst $ filter ((== maxFreq) . snd) count


topKMostCommonBytes :: B.ByteString -> Int -> [Word8]
topKMostCommonBytes bs k =
    let count = sortOn snd (M.toList (countBytes bs))
        lastK = drop (length count - k) count
     in reverse (map fst lastK)


-- Get all possible keys such that the result is in the right charset. (SLOW + BAD)
possibleKeys :: B.ByteString -> [Word8] -> Integer -> [B.ByteString]
possibleKeys ct charset keyLen =
    let ss = offsetStrs ct (fromIntegral keyLen)
        possible = map (`possibleChars` charset) ss
     in map B.pack (sequence possible)

probableKeys :: B.ByteString -> [Word8] -> Integer -> Word8 -> Int -> [B.ByteString]
probableKeys ct charset keyLen mostChar k =
    let ss = offsetStrs ct (fromIntegral keyLen)
        top10 = map (B.pack . ((flip topKMostCommonBytes) k)) ss
        probable = map (bytesXor (B.pack [mostChar])) top10
     in map B.pack (sequence (map B.unpack probable))


-- The normal functions generate all possible keys, the prime (') functions
-- only work properly if there is enough plaintext for basic statistical analysis
-- to work.

-- PT is the partially known plaintext to search for, if none known use B.empty.
bruteForceXor :: B.ByteString -> [Word8] -> Integer -> B.ByteString
                     -> [(B.ByteString, B.ByteString)]
bruteForceXor ct charset keyLen pt =
    filter (\(_, out) -> B.isInfixOf pt out) $
        parMap rpar (\key -> (key, bytesXor key ct))
            (possibleKeys ct charset keyLen)


bruteForceXor' :: B.ByteString -> [Word8] -> Integer -> B.ByteString
                -> Word8 -> Int -> [(B.ByteString, B.ByteString)]
bruteForceXor' ct charset keyLen pt mostChar k =
    filter (\(_, out) -> B.isInfixOf pt out) $
        parMap rpar (\key -> (key, bytesXor key ct))
            (probableKeys ct charset keyLen mostChar k)

-- SW = startsWith, which is the plaintext that this starts with
bruteForceXorSW ct charset keyLen pt startsWith =
    let swLength = B.length startsWith
        newKeyLen = fromIntegral keyLen - swLength
        keyInitial = B.take swLength (bytesXor ct startsWith)
        dropStuff ct
          | B.null ct = ct
          | otherwise = B.append front (dropStuff rest')
            where rest = B.drop swLength ct
                  front = B.take newKeyLen rest
                  rest' = B.drop newKeyLen rest
        newCt = dropStuff ct
        keys = possibleKeys newCt charset (toInteger newKeyLen)
     in filter (\(_, out) -> B.isInfixOf pt out) $
            parMap rpar (\key -> ((B.append keyInitial key),
                                    bytesXor (B.append keyInitial key) ct)) keys

bruteForceXorSW' ct charset keyLen pt startsWith mostChar k =
    let swLength = B.length startsWith
        newKeyLen = fromIntegral keyLen - swLength
        keyInitial = B.take swLength (bytesXor ct startsWith)
        dropStuff ct
          | B.null ct = ct
          | otherwise = B.append front (dropStuff rest')
            where rest = B.drop swLength ct
                  front = B.take newKeyLen rest
                  rest' = B.drop newKeyLen rest
        newCt = dropStuff ct
        keys = probableKeys newCt charset (toInteger newKeyLen) mostChar k
     in filter (\(_, out) -> B.isInfixOf pt out) $
            parMap rpar (\key -> ((B.append keyInitial key),
                                    bytesXor (B.append keyInitial key) ct)) keys
