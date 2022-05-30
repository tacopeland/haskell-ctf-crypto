module NumberTheory.Primes (isPrime, coprime, millerRabin) where

import Helpers
import NumberTheory.Basic
import NumberTheory.Modular

import System.Random (randomRs, mkStdGen)

-- |Tests whether an integer is prime, using 40 Miller-Rabin witnesses.
isPrime :: (Integral a) => a -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = all (millerRabin n)
                (take 40 $ map fromInteger
                               (randomRs (2, toInteger (n - 2))
                               (mkStdGen 0xdeadcafe)))

-- |Test whether the arguments are coprime.
coprime :: (Integral a) => a -> a -> Bool
coprime a b = g == 1
    where (g, _, _) = xgcd a b

-- |Miller-Rabin primality test, given the number to test and a witness.
millerRabin :: (Integral a) => a -> a -> Bool
millerRabin n w
  | y == 1 || y == n - 1 = True
  | otherwise = inner y 1
  where (r, s) = decomposeEven (n - 1)
        Just y = modPow w r n
        inner t j
          | j > (s - 1) && t /= n - 1 = False
          | t == n - 1 = True
          | otherwise = (y' /= 1) && inner y' (j + 1)
          where Just y' = modPow t 2 n
