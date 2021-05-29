module Crypto.Algebra.ZZ where

import Crypto.Algebra.Ring.Class
import qualified Crypto.Algebra.Domain.Class as D


    {-
       Internal typeclasses
    -}

newtype ZZ = ZZ Integer
    deriving (Show, Eq)

instance Ring ZZ where
    rzero _ = ZZ 0
    radd (ZZ a) (ZZ b) = ZZ (a + b)
    rmul (ZZ a) (ZZ b) = ZZ (a * b)
    rneg (ZZ a) = ZZ (-a)
    rpow (ZZ a) b
      | a == -1 = Just $ ZZ (if even b then 1 else (-1))
      | a == 1 && b == -1 = Just (ZZ 1)
      | b >= 0 = Just (ZZ (a ^ b))
      | otherwise = Nothing
    rinv (ZZ a)        = if (a == -1) || (a == 1) then Just (ZZ a) else Nothing

instance IdentityRing ZZ where
    rid _            = ZZ 1

instance CommutativeRing ZZ where

instance D.EuclideanDomain ZZ where
    div = quotRem


    {-
       Prelude typeclasses
    -}

instance Ord ZZ where
    compare (ZZ a) (ZZ b) = compare a b

instance Num ZZ where
    (+)        = radd
    (-) a b    = radd a (rneg b)
    (*)        = rmul
    abs (ZZ a) = ZZ (abs a)
    signum a
      | a < 0  = ZZ (-1)
      | a > 0  = ZZ 1
      | otherwise = ZZ 0
    fromInteger i = ZZ i

instance Enum ZZ where
    toEnum a        = ZZ (toInteger a)
    fromEnum (ZZ a) = fromInteger a

instance Real ZZ where
    toRational (ZZ a) = toRational a

instance Integral ZZ where
    quotRem (ZZ a) (ZZ b) = (ZZ x, ZZ y)
        where (x, y) = quotRem a b
    toInteger (ZZ a) = a
