module Crypto.Integers where

xgcd :: (Integral a) => a -> a -> (a,a,a)
xgcd a 0
    | a < 0 = (-a,-1,0)
    | otherwise = (a,1,0)
xgcd a b =
    let (g, t, t2) = xgcd b (a `mod` b)
    in  (g, t2, t - (a `div` b) * t2)

