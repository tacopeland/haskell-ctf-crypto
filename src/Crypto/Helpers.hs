module Crypto.Helpers where

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
