module Crypto.Ring where

import Data.List

class Ring a where
    rzero :: a -> a
    radd, rmul :: a -> a -> a
    rneg :: a -> a
    rpow :: (Integral i) => a -> i -> Maybe a
    rinv :: a -> Maybe a

class (Ring a) => IdentityRing a where
    rid :: a -> a -- Use with rid :: Z or something

class (Ring a) => CommutativeRing a where

-- Integer ring
data Z = Z Integer
    deriving (Show, Eq)

instance Ring Z where
    rzero _ = (Z 0)
    radd (Z a) (Z b) = Z (a + b)
    rmul (Z a) (Z b) = Z (a * b)
    rneg (Z a) = (Z (-a))
    rpow (Z a) b
      | a == -1 = Just $ Z (if (even b) then 1 else (-1))
      | a == 1 && b == -1 = Just $ Z 1
      | (b >= 0) = Just (Z (a ^ b))
      | otherwise = Nothing
    rinv (Z a)       = if (a == -1) || (a == 1) then Just (Z a) else Nothing

instance IdentityRing Z where
    rid _            = (Z 1)

instance CommutativeRing Z where
