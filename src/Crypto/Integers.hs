-- Misc functions
module Crypto.Integers where


int_xgcd :: (Integral a) => a -> a -> (a,a,a)
int_xgcd a 0
    | a < 0 = (-a,-1,0)
    | otherwise = (a,1,0)
int_xgcd a b =
    let (g, t, t2) = int_xgcd b (a `mod` b)
    in  (g, t2, t - (a `div` b) * t2)

coprime :: (Integral a) => a -> a -> Bool
coprime a b =
    let (g, _, _) = int_xgcd a b in
        if g == 1 then True else False

