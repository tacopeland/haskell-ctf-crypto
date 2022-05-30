module Algebra.Structure.Domain where

import Data.Maybe

import Algebra.Structure.Ring
import Algebra.Structure.Field


class (Ring a, IdentityRing a) => IntegralDomain a where
    divide :: a -> a -> (a, a)

    divides :: a -> a -> Bool
    divides a b = rzero a == snd (divide a b)

class (IntegralDomain a) => GCDDomain a where
    -- TODO: Test this.
    xgcd :: a -> a -> (a, a, a)
    xgcd a b =
        let x = 1
            (g, u, v)
                | b == rzero a = (a, rid a, rzero a)
                | otherwise    =
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
                  
instance IntegralDomain Integer where
    divide = quotRem 

instance GCDDomain Integer where
