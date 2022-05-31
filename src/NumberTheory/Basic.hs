module NumberTheory.Basic (xgcd, divides) where

-- |Extended GCD function. Output is (g, u, v), where g = gcd a b and au + bv = g.
xgcd :: (Integral a) => a -> a -> (a,a,a)
xgcd a 0
    | a < 0 = (-a,-1,0)
    | otherwise = (a,1,0)
xgcd a b =
    let (g, t, t2) = xgcd b (a `mod` b)
     in (g, t2, t - (a `div` b) * t2)

divides :: (Integral a) => a -> a -> Bool
divides a b = b `mod` a == 0
