module Crypto.Field where

import Crypto.Ring
import Crypto.QuotientRing

import Data.Maybe


    {-
        TYPECLASS DEFINITIONS
    -}

class (Ring a) => Field a where
    finv :: a -> a
    finv = fromJust . rinv


    {-
        DATA TYPES AND INSTANCE DEFINITIONS
    -}

instance Field ZnZ where


    {-
        MISCELLANEOUS FUNCTIONS
    -}

 -- |Make a polynomial over a field monic
make_monic :: (Field a) => Rx a -> Rx a
make_monic (Rx a) =
    let leada = last a
        inverse = finv leada
     in scalarmul inverse (Rx a)


