{-# LANGUAGE MultiParamTypeClasses #-}
module Crypto.Algebra.ZZN where

import Data.Maybe

import Crypto.Integers as Int

import Crypto.Algebra.Group.Class
import Crypto.Algebra.Ring.Class
import Crypto.Algebra.Ring.QuotientRing
import Crypto.Algebra.Field.Class

import Crypto.Algebra.ZZ

data ZZN = ZZN { element :: ZZ, modulus :: ZZ }
    deriving (Show, Eq)


    {-
       Internal typeclasses
    -}

-- |Group Z/nZ over addition.
instance Group ZZN where
    gcompose (ZZN a n) (ZZN b n')
      | n == n'   = ZZN ((a + b) `mod` n) n
      | otherwise = error "Conflicting moduli in gcompose"
    gpow (ZZN a n) b 
      | b >= 0   = ZZN ((a + ZZ (toInteger b)) `mod` n) n
      | otherwise = error "Conflicting moduli in gcompose"
    ginv (ZZN a n) = ZZN ((-a) `mod` n) n
    gid (ZZN _ n) = ZZN (ZZ 1) n

instance FiniteGroup ZZN where
    gcardinality (ZZN _ (ZZ n)) = toInteger n
    gorder (ZZN (ZZ a) (ZZ n)) = toInteger (n - a)

instance AbelianGroup ZZN where

instance CyclicGroup ZZN where


-- |Ring Z/nZ.
instance Ring ZZN where
    rzero (ZZN _ n)  = ZZN (ZZ 0) n
    radd = gcompose
    rmul (ZZN a n) (ZZN b n') 
      | n == n' = ZZN ((a * b) `mod` n) n
      | otherwise = error "Conflicting moduli in rmul"
    rneg (ZZN a p) = ZZN ((-a) `mod` p) p
    rpow = zznPow
    rinv = zznModInv

zznPow :: (Integral a) => ZZN -> a -> Maybe ZZN
zznPow n x
  | x >= 0            = Just (foldr gcompose (gid n) (zipWith (\(ZZN a p) e -> ZZN ((a^e) `mod` p) p) (squares rmul n) (binexpand x)))
  | isNothing inverse = Nothing
  | otherwise         = Just $ gpow new_n (-x)
    where inverse = rinv n
          Just new_n = inverse
          squares mult = iterate (\x -> x `mult` x)

zznModInv :: ZZN -> Maybe ZZN
zznModInv (ZZN a p) =
    let (g, x, _) = Int.xgcd a p
    in if g == 1
          then Just (ZZN (x `mod` p) p)
          else Nothing


instance IdentityRing ZZN where
    rid (ZZN a n) = ZZN (ZZ 1) n

instance FiniteRing ZZN where
    rcardinality (ZZN a n) = toInteger (eulerPhi n)
    rorder = rcardinality

instance QuotientRing ZZN ZZ where
    qrelement = element
    qrideal = modulus
    qrcoerce a n = ZZN (a `mod` n) n


    {-
        Prelude typeclasses
    -}

instance Ord ZZN where
    compare (ZZN a n) (ZZN b n')
      | n == n' = compare a b
      | otherwise = error "Conflicting moduli in compare"


instance Num ZZN where
    (+)        = radd
    negate a   = rneg a
    (*)        = rmul
    abs a      = a
    signum a
      | a == rzero a = a
      | otherwise    = rid a
    fromInteger a = error "fromInteger not implemented for ZZN"
