{-# LANGUAGE MultiParamTypeClasses #-}
module Algebra.ZZP (ZZP(..), modSqrt) where

import Internal.Helpers
import NumberTheory.Basic (xgcd)
import NumberTheory.Factor (factor, pollardRhoF)
import NumberTheory.Primes (isPrime)
import NumberTheory.Integers (legendreSymbol, quadraticResidue)

import Algebra.Structure.Group
import Algebra.Structure.Ring
import Algebra.Structure.Field

import Algebra.ZZ

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
  | x >= 0    = squareAndMultiply gcompose (gid n) n x
  | otherwise = gpow new_n (-x)
  where new_n = ginv n

zzpModInv :: ZZP -> ZZP
zzpModInv (ZZP a p) = ZZP (x `mod` p) p
    where (_, x, _) = xgcd a p

order n@(ZZP a p)
  | a == ZZ 0 = error "Multiplicative order of zero not defined!"
  | a == ZZ 1 = 1
  | otherwise =
      let ord = toInteger p - 1
          factors = factor pollardRhoF ord
          inner ord [] = ord
          inner ord (f:factors) = if zzpPow n (ord `div` f) == gid n
                                     then inner (ord `div` f) factors
                                     else inner ord factors
       in inner ord factors



instance FiniteGroup ZZP where
    gcardinality n = toInteger (modulus n) - 1
    gorder         = order

instance AbelianGroup ZZP where

instance CyclicGroup ZZP where

instance Part3 ZZP where
    classify (ZZP (ZZ a) (ZZ p))
      | a <= div p 3     = 1
      | a <= 2 * div p 3 = 2
      | otherwise        = 3


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

instance FiniteRing ZZP where
    rcardinality (ZZP _ p) = toInteger p
    rorder                 = order

instance QuotientRing ZZP ZZ where
    qrelement = element
    qrideal = modulus
    qrcoerce a p
      | isPrime p = ZZP (a `mod` p) p
      | otherwise = error ("Trying to coerce a ZZP with composite modulus: " ++ show p)


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
    negate     = rneg
    (*)        = rmul
    abs a      = a
    signum a
      | a == rzero a = a
      | otherwise    = rid a
    fromInteger a = error "fromInteger not implemented for ZZP"


    {-
        Miscellaneous functions
    -}

-- |The Tonelli-Shanks algorithm for calculating square roots in GF(p)
modSqrt :: ZZP -> [ZZP]
modSqrt n@(ZZP a p)
  | legendreSymbol a p /= 1 = []
  | toInteger p `mod` 4 == 3 =
    let Just root = rpow (ZZP a p) ((p + 1) `div` 4)
     in [root, rneg root]
  | null nonresidues = []
  | otherwise = inner c r 1
  where (q, s) = decomposeEven (p - 1)
        nonresidues = filter (not . flip quadraticResidue p) (map ZZ [2..(toInteger p - 1)])
        z = head nonresidues
        Just inv = rinv n
        Just c = rpow (ZZP z p) q
        Just r = rpow n ((q + 1) `div` 2)
        inner c r i
          | i == s = [r, rneg r]
          | d == rneg (rid n) = inner (c * c) (r * c) (i + 1)
          | otherwise = inner (c * c) r (i + 1)
          where Just d = rpow (r * r * inv) (2^(s - i - 1))
