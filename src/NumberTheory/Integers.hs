-- Misc functions
module NumberTheory.Integers where

import Helpers
import NumberTheory.Basic
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

ntt :: [Integer] -> Integer -> Integer -> [Integer]
ntt vec omega modulus = xs
    where
        n = toInteger $ length vec
        omegas i = consecutiveModPowers (mPow omega i modulus) modulus
        xs = map ((`modSum` modulus) . zipWith (*) vec . omegas) [0..(n-1)]

inverseNtt :: [Integer] -> Integer -> Integer -> [Integer]
inverseNtt vec omega modulus = xs
    where
        n = toInteger $ length vec
        xs' = ntt vec (mInv omega modulus) modulus
        xs = map (\x -> modMul x (mInv n modulus) modulus) xs'

-- |There is a primitive nth root of unity in ZZp iff n | p-1.
-- This assumes there is an nth root of unity.
primitiveRootOfUnity :: Integer -> Integer -> Maybe Integer
primitiveRootOfUnity n modulus
  | n `divides` (modulus-1) =
    let stdGen = mkStdGen 0xdeadcafe
        nFacs = nub (factor pollardP1 n)
        inner gen =
            let (a, gen') = randomR (2, modulus-1) gen
             in if mPow a n modulus == 1
                   && (1 `notElem` map (\i -> mPow a (n `div` i) modulus) nFacs)
                   then a
                   else inner gen'
    in Just $ inner stdGen
  | otherwise = Nothing
