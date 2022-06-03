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


    {-
       Attacks
    -}

scoreElems ct rFreqTable =
    let
        sqDist a b = abs (a - b)

        unsafeLookup :: (Eq k) => k -> [(k,v)] -> v
        unsafeLookup k m = fromJust $ lookup k m

        freqs = freqTable ct

        relFreqs =
            map (\(char, freq) ->
                  (char, fromIntegral freq / fromIntegral (length ct)))
                freqs

        scoreE (c, f) =
            (c, filter (\x -> snd x <= 0.01)
                (map (BF.second (sqDist f)) rFreqTable))

        scoreEs = map scoreE relFreqs
        
     in scoreEs

scoreMonogramsEnglish :: [Char] -> [(Char, [(Char, Float)])]
scoreMonogramsEnglish ct = scoreElems ct englishCharFreqs

generateKeys :: [(Char, [Char])] -> [T.Text]
generateKeys toGen =
    let
        toGen' :: [[Char]]
        toGen' = map snd (L.sortOn fst toGen)
     in map T.pack (sequence toGen')

-- When breaking this, do the following:
-- Create a frequency table for each letter in the ciphertext,
-- with punctuation removed and turned to lowercase.
-- For each letter, sort the alphabet by (abs (- (freq letter) (freq c)))
-- Generate possible keys in that order.
-- Write a function that accepts a list of lists and gives all lists that take
-- 1 from each of those lists, but make sure it follows the original ordering in some way.
-- For instance, it takes all the first elements, then it takes from either the first or second,
-- then from one of the first three, then one of the first 4, etc.
--attackEnglish :: T.Text -> T.Text -> T.Text -> T.Text -> [(Char, [Char])]
attackEnglish ct searchTerm alpha1 alpha2 =
    let chars = T.unpack ct
        lowers = map C.toLower chars
        ctMonograms = filter C.isAlpha lowers
        monogramScores = scoreMonogramsEnglish ctMonograms
        toGen' :: [(Char, [Char])]
        toGen' = map (\(c, chars) -> (c, map fst (L.sortOn snd chars)))
                    monogramScores
        hints :: M.Map Char [Char]
        hints = M.fromList (map (\(x,y) -> (x, [y]))
                                (T.zip alpha1 alpha2))
        toGen :: [(Char, [Char])]
        toGen = map (\x -> (fst x, M.findWithDefault (snd x) (fst x) hints)) toGen'
     in filter (T.isInfixOf searchTerm . snd)
                 (map (\key -> (key, txtSubstitute ct asciiLower key))
                      (generateKeys toGen))
