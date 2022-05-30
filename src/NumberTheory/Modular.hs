module NumberTheory.Modular (modPow, modInv) where

import Helpers
import NumberTheory.Basic

modPow :: (Integral a) => a -> a -> a -> Maybe a
modPow a x n
    | x >= 0        = Just (squareAndMultiply (\x y -> (x * y) `mod` n) 1 a x)
    | otherwise     = modInv a n >>= (\inv -> modPow inv (-x) n)

modInv :: (Integral a) => a -> a -> Maybe a
modInv a n
  | g == 1    = Just (x `mod` n)
  | otherwise = Nothing
    where (g, x, _) = xgcd a n

