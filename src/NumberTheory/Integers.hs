-- Misc functions
module NumberTheory.Integers where

import Helpers
import NumberTheory.Factor
import NumberTheory.Modular
import NumberTheory.Primes

import Data.List
import Data.Maybe
import System.Random


jacobiSymbol :: (Integral a) => a -> a -> a
jacobiSymbol a' n'
  | even n' = error "Second input to jacobiSymbol must be odd."
  | a' == 0 = 0
  | a' == 1 = 1
  | otherwise = if a == 1 then s else s * jacobiSymbol n a
  where (a, e) = decomposeEven a'
        s' | even e = 1
           | (n' `mod` 8) `elem` [1,7] = 1
           | otherwise = -1
        s = if (n' `mod` 4) == 3 && (a `mod` 4) == 3
                  then (-s')
                  else s'
        n = n' `mod` a

legendreSymbol :: (Integral a) => a -> a -> a
legendreSymbol a p
  | isPrime p = jacobiSymbol a p
  | otherwise = error "Second input to legendreSymbol must be prime."

-- |Is `a` a quadratic residue modulo `p`?
quadraticResidue :: (Integral a) => a -> a -> Bool
quadraticResidue a p
  | not (isPrime p) =
      error "Quadratic residue checking for nonprime moduli not implemented."
  | s == 1          = True 
  | otherwise       = False
  where s = legendreSymbol a p

-- |n * product (1 - 1/p)
eulerPhi :: Integer -> Integer
eulerPhi 1 = 1
eulerPhi n = product (map (\x -> n - n `div` x) factors) `div` n^(l - 1)
    where factors = nub (factor pollardRhoF (toInteger n))
          l = length factors
