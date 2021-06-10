module Crypto.Helpers where

import Data.List
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
