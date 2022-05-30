module Algebra.Structure.Group where

    {-
        TYPECLASS DEFINITIONS
    -}

class Group a where
    gcompose :: a -> a -> a
    gpow :: (Integral i) => a -> i -> a
    ginv :: a -> a
    gid :: a -> a

class FiniteGroup a where
    gcardinality :: a -> Integer
    gorder :: a -> Integer

class AbelianGroup a where

class CyclicGroup a where

-- |For groups whose sets can be partitioned into 3 of roughly equal length, for use in Pollard's Rho discrete logarithms.
class Part3 a where
    classify :: a -> Integer
