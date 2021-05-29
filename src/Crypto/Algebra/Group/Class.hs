module Crypto.Algebra.Group.Class where

import Data.List
import Data.Maybe

import Crypto.Integers


    {-
        TYPECLASS DEFINITIONS
    -}

class Group a where
    gcompose :: a -> a -> a
    gpow :: (Integral i) => a -> i -> a
    ginv :: a -> a
    gid :: a -> a

class FiniteGroup a where
    gorder :: a -> Integer

class AbelianGroup a where

class CyclicGroup a where
