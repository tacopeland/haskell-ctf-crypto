module Crypto.Integers where

-- Misc functions

xgcd :: (Integral a) => a -> a -> (a,a,a)
xgcd a 0
    | a < 0 = (-a,-1,0)
    | otherwise = (a,1,0)
xgcd a b =
    let (g, t, t2) = xgcd b (a `mod` b)
    in  (g, t2, t - (a `div` b) * t2)

coprime :: (Integral a) => a -> a -> Bool
coprime a b =
    let (g, _, _) = xgcd a b in
        if g == 1 then True else False

