-- Misc functions
module Crypto.Integers where

import Data.Maybe
import System.Random


-- |Extended GCD function. Output is (g, u, v), where g = gcd a b and au + bv = g.
xgcd :: (Integral a) => a -> a -> (a,a,a)
xgcd a 0
    | a < 0 = (-a,-1,0)
    | otherwise = (a,1,0)
xgcd a b =
    let (g, t, t2) = xgcd b (a `mod` b)
     in (g, t2, t - (a `div` b) * t2)

-- |Test whether the arguments are coprime.
coprime :: (Integral a) => a -> a -> Bool
coprime a b = g == 1
    where (g, _, _) = xgcd a b

-- |Expand an integer into its binary representation.
binexpand :: (Integral a) => a -> [a]
binexpand 0 = []
binexpand a 
  | even a    = 0 : leftover
  | otherwise = 1 : leftover
  where leftover = binexpand $ a `div` 2

modPow :: (Integral a) => a -> a -> a -> Maybe a
modPow a x n
    | x >= 0              = Just $ foldr (\x y -> (x * y) `mod` n) 1
                            (zipWith (\a e -> (a^e) `mod` n)
                                (iterate (\x -> (x * x) `mod` n) a)
                                (binexpand x))
    | isNothing res       = Nothing
    | otherwise           = modPow inverse (-x) n
    where res = modInv a n
          Just inverse = res

modInv :: (Integral a) => a -> a -> Maybe a
modInv a n
  | g == 1    = Just (x `mod` n)
  | otherwise = Nothing
    where (g, x, _) = xgcd a n




decomposeEven :: (Integral a) => (a, a) -> (a, a)
decomposeEven (a, e)
          | even a    = decomposeEven (a `div` 2, e + 1)
          | otherwise = (a, e)


jacobiSymbol :: (Integral a) => a -> a -> a
jacobiSymbol a' n'
  | even n' = error "Second input to jacobiSymbol must be odd."
  | a' == 0 = 0
  | a' == 1 = 1
  | otherwise = if a == 1 then s else s * jacobiSymbol n a
  where (a, e) = decomposeEven (a', 0)
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

quadraticResidue :: (Integral a) => a -> a -> Bool
quadraticResidue a p
  | not (isPrime p) = error "Quadratic residue checking for nonprime moduli not implemented."
  | s == 1 = True 
  | otherwise = False
  where s = legendreSymbol a p


-- |Tests whether an integer is prime, using 50 Miller-Rabin witnesses.
isPrime :: (Integral a) => a -> Bool
isPrime 2 = True
isPrime n = all (millerRabin n) (take 40 $ map fromInteger (randomRs (2, toInteger (n - 2)) (mkStdGen 0xdeadcafe)))

-- |Miller-Rabin primality test, given the number to test and a witness.
millerRabin :: (Integral a) => a -> a -> Bool
millerRabin n w
  | y == 1 || y == n - 1 = True
  | otherwise = inner y 1
  where (r, s) = decomposeEven (n - 1, 0)
        Just y = modPow w r n
        inner t j
          | j > (s - 1) && t /= n - 1 = False
          | t == n - 1 = True
          | otherwise = (y' /= 1) && inner y' (j + 1)
          where Just y' = modPow t 2 n
