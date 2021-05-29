-- |Domains. Intended to be imported qualified, as 
{-# LANGUAGE FlexibleInstances #-}
module Crypto.Algebra.Domain.Class where

import Prelude hiding (div)

import Data.Maybe

import Crypto.Algebra.Ring.Class
import Crypto.Algebra.Field.Class


class (Ring a, IdentityRing a) => EuclideanDomain a where
    div :: a -> a -> (a, a)
    divides :: a -> a -> Bool
    divides a b = rzero a == snd (div a b)
    xgcd :: a -> a -> (a, a, a)
    xgcd a b =
        let x = 1
            (g, u, v)
                | b == rzero a        = (a, rid a, rzero a)
                | otherwise           =
                    let (q, r) = div a b
                        (g, t, t2) = xgcd b r
                    in  (g, t2, t `radd` (rneg q `rmul` t2))
            inverse = rinv g
            Just inv = inverse
         in if isJust inverse
            then (rmul inv g, rmul inv u, rmul inv v)
            else (g, u, v)
    mod :: a -> a -> a
    mod a b = let (q, r) = div a b
                  in r


    {-
        DATA TYPES AND INSTANCE DEFINITIONS
instance (IdentityRing a) => EuclideanDomain (Rx a) where
    div (Rx a) (Rx b) =
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
    -}
