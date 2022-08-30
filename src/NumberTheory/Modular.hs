module NumberTheory.Modular
    (modPow,
    mPow,
    modInv,
    mInv,
    modMul,
    modSum
    )
    where

import Internal.Helpers
import NumberTheory.Basic

import Data.Maybe

modPow :: (Integral a) => a -> a -> a -> Maybe a
modPow a x n
    | x >= 0        = Just (squareAndMultiply (\x y -> (x * y) `mod` n) 1 a x)
    | otherwise     = modInv a n >>= (\inv -> modPow inv (-x) n)

-- |Unsafe version of modPow, use when you know you are working with a prime modulus.
mPow :: (Integral a) => a -> a -> a -> a
mPow a x n = fromJust (modPow a x n)

modInv :: (Integral a) => a -> a -> Maybe a
modInv a n
  | g == 1    = Just (x `mod` n)
  | otherwise = Nothing
    where (g, x, _) = xgcd a n

-- |Unsafe version of modInv, use when you know you are working with a prime modulus.
mInv :: (Integral a) => a -> a -> a
mInv a n = fromJust (modInv a n)


-- These are just for cleaning up code (removing some need for parentheses)

modMul :: (Integral a) => a -> a -> a -> a
modMul a b = mod (a * b)

modSum :: (Integral a) => [a] -> a -> a
modSum xs = mod (sum xs)
