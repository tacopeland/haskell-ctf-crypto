-- Misc functions
module Crypto.Integers where


intXgcd :: (Integral a) => a -> a -> (a,a,a)
intXgcd a 0
    | a < 0 = (-a,-1,0)
    | otherwise = (a,1,0)
intXgcd a b =
    let (g, t, t2) = intXgcd b (a `mod` b)
     in (g, t2, t - (a `div` b) * t2)

coprime :: (Integral a) => a -> a -> Bool
coprime a b =
    let (g, _, _) = intXgcd a b
     in g == 1

binexpand :: (Integral a) => a -> [a]
binexpand 0 = []
binexpand a 
  | even a    = 0 : leftover
  | otherwise = 1 : leftover
  where leftover = binexpand $ a `div` 2
