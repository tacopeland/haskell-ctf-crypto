{-# LANGUAGE FlexibleInstances #-}
module Crypto.Domain where

import Data.Maybe

import Crypto.Ring
import Crypto.Field


    {-
        TYPECLASS DEFINITIONS
    -}

class (Ring a, IdentityRing a) => EuclideanDomain a where
    divide :: a -> a -> (a, a)
    divides :: a -> a -> Bool
    divides a b = rzero a == snd (divide a b)
    xgcd :: a -> a -> (a, a, a)
    xgcd a b =
        let x = 1
            (g, u, v)
                | b == rzero a        = (a, rid a, rzero a)
                | otherwise           =
                    let (q, r) = divide a b
                        (g, t, t2) = xgcd b r
                    in  (g, t2, t `radd` (rneg q `rmul` t2))
            inverse = rinv g
            Just inv = inverse
         in if isJust inverse
            then (rmul inv g, rmul inv u, rmul inv v)
            else (g, u, v)
    modulo :: a -> a -> a
    modulo a b = let (q, r) = divide a b
                  in r


    {-
        DATA TYPES AND INSTANCE DEFINITIONS
    -}

instance EuclideanDomain Z where
    divide (Z a) (Z b) = (Z (a `div` b), Z (a `mod` b))

instance (IdentityRing a) => EuclideanDomain (Rx a) where
    divide (Rx a) (Rx b) =
        let normalize x = reverse $ dropWhile (== rzero (head a)) (reverse x)
            degree x = length (normalize x) - 1
            div_iter a b
              | degree a < degree b = ([], a)
              | otherwise =
                let zero = rzero (head a)
                    mulxn a n = replicate n zero ++ a
                    leada = last a
                    leadb = last b
                    x = rmul (fromJust (rinv leadb)) leada
                    Rx new_a = radd (Rx a) (rneg (scalarmul x (Rx (mulxn b (degree a - degree b)))))
                    (q, r) = div_iter (normalize new_a) b
                 in (x : q, r)
            (q, r) = div_iter (normalize a) (normalize b)
         in (Rx (reverse q), Rx r)
