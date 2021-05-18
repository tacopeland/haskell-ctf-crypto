module Crypto.Domain where

import Crypto.Ring


    {-
        TYPECLASS DEFINITIONS
    -}

class (Ring a, IdentityRing a, Eq a) => EuclideanDomain a where
    divide :: a -> a -> (a, a)
    divides :: a -> a -> Bool
    divides a b = rzero a == (snd $ divide a b)
    xgcd :: a -> a -> (a, a, a)
    xgcd a b
        | b == rzero a        = (a, rid a, rzero a)
        | otherwise           =
            let (q, r) = divide a b
                (g, t, t2) = xgcd b r
            in  (g, t2, t `radd` (rneg q `rmul` t2))


    {-
        DATA TYPES AND INSTANCE DEFINITIONS
    -}

instance EuclideanDomain Z where
    divide (Z a) (Z b) = (Z (a `div` b), Z (a `mod` b))
