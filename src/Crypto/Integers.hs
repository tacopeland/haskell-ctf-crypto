-- Misc functions
module Crypto.Integers where


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

isPrime :: (Integral a) => a -> Bool
isPrime = millerRabin

-- Placeholder for Miller-Rabin primality test
millerRabin :: (Integral a) => a -> Bool
millerRabin a = True

