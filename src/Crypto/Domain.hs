{-# LANGUAGE FlexibleInstances #-}
module Crypto.Domain where

import Data.Maybe

import Crypto.Ring
import Crypto.QuotientRing


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
    modulo :: a -> a -> a
    modulo a b = let (q, r) = divide a b
                  in r


    {-
        DATA TYPES AND INSTANCE DEFINITIONS
    -}

instance EuclideanDomain Z where
    divide (Z a) (Z b) = (Z (a `div` b), Z (a `mod` b))

instance EuclideanDomain (Rx ZnZ) where
    divide (Rx a) (Rx b) =
        let normalize x = reverse $ dropWhile (== (rzero (head a))) (reverse x)
            degree x = (length (normalize x)) - 1
            div_iter a b
              | degree a < degree b = ([], a)
              | otherwise =
                let (ZnZ _ n) = head a
                    zero = rzero (head a)
                    mulxn a n = (take n (repeat zero)) ++ a
                    leada = last a
                    leadb = last b
                    x = rmul (fromJust (rinv leadb)) leada
                    Rx new_a = radd (Rx a) (rneg (scalarmul x (Rx (mulxn b (degree a - degree b)))))
                    (q, r) = div_iter (normalize new_a) b
                 in (x : q, r)
            (q, r) = div_iter (normalize a) (normalize b)
         in (Rx (reverse q), Rx r)
