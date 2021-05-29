module Crypto.Algebra.Field.Class where

import Crypto.Algebra.Ring.Class

import Data.Maybe


class (IdentityRing a) => Field a where
    finv :: a -> a
    finv = fromJust . rinv
    fpow :: (Integral b) => a -> b -> a
    fpow a = fromJust . rpow a


    {-
-- |Make a polynomial over a field monic
makeMonic :: (Field a) => Rx a -> Rx a
makeMonic (Rx a) =
    let leada = last a
        inverse = finv leada
     in scalarmul inverse (Rx a)


-}
