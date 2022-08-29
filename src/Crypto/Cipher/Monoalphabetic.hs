module Crypto.Cipher.Monoalphabetic where

import Helpers
import Crypto.Util.Charset
import Crypto.Util.Langs

import qualified Data.Bifunctor as BF
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Text as T


subTable :: T.Text -> T.Text -> M.Map Char Char
subTable alpha1 alpha2 = M.fromList $ zip (T.unpack alpha1) (T.unpack alpha2)

txtSubstituteMap :: T.Text -> M.Map Char Char -> T.Text
txtSubstituteMap txt subTable = T.map (\e -> M.findWithDefault e e subTable) txt

englishKeyExpand :: T.Text -> T.Text
englishKeyExpand initkey = T.pack (L.nub (T.unpack (T.append (T.toLower initkey) asciiLower)))

-- Used when keyed in the traditional way.
encryptEnglish :: T.Text -> T.Text -> T.Text
encryptEnglish pt initkey =
    let key = englishKeyExpand initkey
        alpha1 = T.append key (T.toUpper key)
        alpha2 = T.append asciiLower asciiUpper
     in txtSubstituteMap pt (subTable alpha1 alpha2)


decryptEnglish :: T.Text -> T.Text -> T.Text
decryptEnglish pt initkey =
    let key = englishKeyExpand initkey
        alpha1 = T.append key (T.toUpper key)
        alpha2 = T.append asciiLower asciiUpper
     in txtSubstituteMap pt (subTable alpha2 alpha1)


-- This is an analogue of the UNIX `tr` command.
txtSubstitute :: T.Text -> T.Text -> T.Text -> T.Text
txtSubstitute pt alpha1 alpha2 = txtSubstituteMap pt (subTable alpha1 alpha2)


rot n txt = txtSubstituteMap txt st
    where lowercase = T.drop n asciiLower `T.append` T.take n asciiLower
          full = lowercase `T.append` T.toUpper lowercase
          st = subTable (asciiLower `T.append` asciiUpper) full

-- The closer to zero, the better the match
matchFreqs :: [(Char, Float)] -> [(Char, Float)] -> Float
matchFreqs l r = foldr (\(x, y) -> (+) ((x - y) ^ 2)) 0.0 matched
  where matched = [(snd a, snd b) | a <- l, b <- r, fst a == fst b]

matchesEnglish :: T.Text -> Float
matchesEnglish t = matchFreqs tFreqs englishCharFreqs
    where cleanT = T.filter C.isLower (T.toLower t)
          tFreqs = relFreqTable cleanT
          relFreqTable t = map (fmap (\y -> fromIntegral y / fromIntegral (T.length t)))
                               (freqTable (T.unpack t))

-- Whether this is just a substituted English plaintext
matchesSubstitutedEnglish :: T.Text -> Float
matchesSubstitutedEnglish t = foldr (\(x, y) -> (+) ((x - y) ^ 2)) 0.0 (zip sortedFreqs sortedEngFreqs)
    where cleanT = T.filter C.isLower (T.toLower t)
          tFreqs = relFreqTable cleanT
          relFreqTable t = map (fmap (\y -> fromIntegral y / fromIntegral (T.length t)))
                               (freqTable (T.unpack t))
          sortedFreqs = map snd (L.sortOn snd tFreqs)
          sortedEngFreqs = map snd (L.sortOn snd englishCharFreqs)


    {-
       Attacks
    -}
-- hillClimb state fitness isGoal newState nextGuess
