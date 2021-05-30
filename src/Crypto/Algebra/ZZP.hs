{-# LANGUAGE MultiParamTypeClasses #-}
module Crypto.Algebra.ZZP where

import Crypto.Integers as Int

import Crypto.Algebra.Group.Class
import Crypto.Algebra.Ring.Class
import Crypto.Algebra.Ring.QuotientRing
import Crypto.Algebra.Field.Class

import Crypto.Algebra.Generic
import Crypto.Algebra.ZZ

data ZZP = ZZP { element :: ZZ, modulus :: ZZ }
    deriving (Show, Eq)


    {-
       Internal typeclasses
    -}

-- |Group Z/pZ over multiplication.
instance Group ZZP where
    gcompose (ZZP a p) (ZZP b p')
        | p == p'   = ZZP ((a * b) `mod` p) p
        | otherwise = error "Conflicting moduli in gcompose"
    gpow = zzpPow
    ginv = zzpModInv
    gid (ZZP _ p) = ZZP (ZZ 1) p

zzpPow :: (Integral a) => ZZP -> a -> ZZP
zzpPow n x
    | x >= 0              = foldr gcompose (gid n)
                            (zipWith (\(ZZP a p) e -> ZZP ((a^e) `mod` p) p)
                                (squares gcompose n)
                                (binexpand x))
    | otherwise           = gpow new_n (-x)
    where new_n = ginv n

zzpModInv :: ZZP -> ZZP
zzpModInv (ZZP a p) = ZZP (x `mod` p) p
    where (_, x, _) = Int.xgcd a p

instance FiniteGroup ZZP where
    gorder (ZZP (ZZ 0) _) = error "Invalid ZZP: there is no zero element in ZZP."
    gorder n
      | n == gid n        = 1
      | otherwise         = toInteger (modulus n) - 1

instance AbelianGroup ZZP where

instance CyclicGroup ZZP where

-- |Ring Z/pZ.
instance Ring ZZP where
    rzero (ZZP _ p)  = ZZP (ZZ 0) p
    radd (ZZP a p) (ZZP b p')
        | p == p'    = ZZP ((a + b) `mod` p) p
        | otherwise  = error "Conflicting moduli in radd"
    rmul = gcompose
    rneg (ZZP a p) = ZZP ((-a) `mod` p) p
    rpow a                 = Just . gpow a
    rinv                   = Just . ginv

instance IdentityRing ZZP where
    rid (ZZP a p) = ZZP (ZZ 1) p

instance QuotientRing ZZP ZZ where
    qrelement = element
    qrideal = modulus
    qrcoerce a p
      | Int.isPrime p = ZZP (a `mod` p) p
      | otherwise = error "Trying to coerce a ZZP with composite modulus."


instance Field ZZP where


    {-
        Prelude typeclasses
    -}

instance Ord ZZP where
    compare (ZZP a n) (ZZP b n')
      | n == n' = compare a b
      | otherwise = error "Conflicting moduli in compare"


instance Num ZZP where
    (+)        = radd
    negate a   = rneg a
    (*)        = rmul
    abs a      = a
    signum a
      | a == rzero a = a
      | otherwise    = rid a
    fromInteger a = error "fromInteger not implemented for ZZP"
