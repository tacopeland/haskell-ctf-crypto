module Crypto.Algebra.Factor where

import Crypto.Integers as I

import Math.NumberTheory.Roots
import System.Random

-- |Pollard's p-1 factorization method for when `p-1` is a product of small factors.
pollardP1 :: Integer -> Integer
pollardP1 n = inner 2 2 n
    where inner j a n =
            let Just a' = modPow a j n
                d  = gcd (a' - 1) n
             in if d > 1 && d < n
                   then d
                   else inner (j + 1) a' n

-- |Fermat's factorization method for when p - q < n^(1/4).
fermatFactor :: Integer -> Integer
fermatFactor n = inner a b2
    where a = toInteger (ceiling (sqrt (fromIntegral n)))
          b2 = a * a - n
          inner x y
            | isSquare y = x - integerSquareRoot y
            | otherwise = inner (x + 1) (y + 2 * x + 1)

-- |Pollard's Rho factorization method, with mixing function f(x) = x^2 + 1 (mod n).
pollardRho :: Integer -> Integer
pollardRho n = inner x1 x1 c gen1
    where (x1, gen1) = randomR (2, n - 1) (mkStdGen 0xdeadcafe)
          (c, gen2)  = randomR (2, n - 1) gen1
          inner x y c gen
            -- If this value of x is getting us nowhere
            | x `elem` [0, 1, xi] || d >= n =
                let (newC, newGen) = randomR (2, n - 1) gen
                 in inner xi yi newC newGen
            | d > 1 = d
            | otherwise = inner xi yi c gen
           where mix a = (modSquare a n + c) `mod` n
                 xi = mix x
                 yi = mix (mix y)
                 d = gcd ((yi - xi) `mod` n) n

-- |Takes as input a function that returns a single factor and a number and fully factorizes that number.
factor :: (Integer -> Integer) -> Integer -> [Integer]
factor f n
  | isPrime n = [n]
  | even n = 2 : factor f (n `div` 2)
  | otherwise =
      let p = f n
          q = n `div` p
       in factor f p ++ factor f q
