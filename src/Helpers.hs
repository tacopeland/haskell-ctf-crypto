module Helpers where

import Data.Bits ((.&.))
import Data.List
import qualified Data.Map as Map
import Data.Maybe

-- |Generates a list of squares given a multiplication function.
squares :: (a -> a -> a) -> a -> [a]
squares mult = iterate (\x -> x `mult` x)

-- |Expand an integer into its binary representation.
binexpand :: (Integral a) => a -> [a]
binexpand 0 = []
binexpand a 
  | even a    = 0 : leftover
  | otherwise = 1 : leftover
  where leftover = binexpand $ a `div` 2

-- |Generic square-and-multiply algorithm.
squareAndMultiply :: (Integral b) => (a -> a -> a) -> a -> a -> b -> a
squareAndMultiply operation identity base exponent =
    foldr operation identity
        (zipWith (\a e -> if e == 1 then a else identity)
            (squares operation base)
            (binexpand exponent))

-- |Generic collision algorithm. Expects that both lists are of the
-- same size and contain no duplicates.
collide :: (Eq a) => [a] -> [a] -> [a] -> Int -> Maybe (a, Int, Int)
collide (x:xs) (y:ys) l index
  | x == y       = Just (x, index, index)
  | x `elem` l = Just (x, index, getInd x `div` 2)
  | y `elem` l = Just (y, getInd y `div` 2, index)
  | otherwise = collide xs ys (x:y:l) (index + 1)
  where getInd = (-) (length l - 1) . fromJust . flip elemIndex l
collide _ _ _ _ = Nothing


-- |Get frequency analysis of a list.
freqTable :: (Ord a) => [a] -> [(a, Integer)]
freqTable l = Map.toList (Map.fromListWith (+) [(c, 1) | c <- l])

-- |Get local maxima of a list.
localMaxima :: (Ord a) => [a] -> [a]
localMaxima [] = []
localMaxima [x] = [x]
localMaxima [x, y]
  | y > x = [y]
  | otherwise = []
localMaxima (x:y:z:rest)
  | y > x && y > z = y : localMaxima (y:z:rest)
  | otherwise = localMaxima (y:z:rest)

-- |Turn a number of the form n into a*2^e. 
decomposeEven :: (Integral a) => a -> (a, a)
decomposeEven n = inner (n, 0)
    where inner (a, e)
            | even a && a > 0 = inner (a `div` 2, e + 1)
            | otherwise       = (a, e)

-- |http://www.graphics.stanford.edu/~seander/bithacks.html#DetermineIfPowerOf2
powerOf2 :: Integer -> Bool
powerOf2 a = a .&. (a - 1) == 0

nextPowerOf2 :: Integer -> Integer
nextPowerOf2 a
  | a <= 1    = 1
  | otherwise = 2 ^ (floor (logBase 2 (fromIntegral (a-1))) + 1)

consecutivePowers :: Integer -> [Integer]
consecutivePowers k = iterate (*k) 1

consecutiveModPowers :: Integer -> Integer -> [Integer]
consecutiveModPowers k n = iterate ((`mod` n) . (*k)) 1
