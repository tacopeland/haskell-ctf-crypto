module Algebra.Structure.Field where

import Algebra.Structure.Ring

import Data.Maybe


class (IdentityRing a) => Field a where
    finv :: a -> a
    finv = fromJust . rinv
    fpow :: (Integral b) => a -> b -> a
    fpow a = fromJust . rpow a
