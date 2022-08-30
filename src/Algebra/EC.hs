module Algebra.EC
    (EC(..),
    (.+),
    (.*),
    liftX)
    where

import Internal.Helpers

import Algebra.Structure.Group
import Algebra.Structure.Ring

import Algebra.ZZ
import Algebra.ZZP

import Data.Maybe

-- |The elliptic curve group over GF(p).
-- This takes the Weierstrass form Y^2 = X^3 + a*X + b
data EC = EC_O | EC { ecX :: ZZP, ecY :: ZZP, ecA :: ZZP, ecB :: ZZP }
    deriving (Show, Eq)


    {-
       Internal typeclasses
    -}

instance Group EC where
    gcompose p@(EC x1 y1 a b) q@(EC x2 y2 a' b')
      | a /= a' || b /= b' = error "Adding points on two different elliptic curves."
      | x1 == x2 && y1 == rneg y2 = EC_O
      | otherwise =
          let two = radd (rid x1) (rid x1)
              three = radd (rid x1) (radd (rid x1) (rid x1))
              lambda = if p /= q
                          then rmul (radd y2 (rneg y1)) (fromJust $ rinv (radd x2 (rneg x1)))
                          else rmul (radd (rmul three (rmul x1 x1)) a) (fromJust $ rinv (rmul two y1))
              x3 = radd (rmul lambda lambda) (rneg (radd x1 x2))
              y3 = radd (rmul lambda (radd x1 (rneg x3))) (rneg y1)
           in EC x3 y3 a b
    gcompose EC_O q = q
    gcompose p EC_O = p
    gpow = ecpow
    ginv EC_O = EC_O
    ginv (EC x y a b) = EC x (rneg y) a b
    gid _ = EC_O

ecpow :: (Integral a) => EC -> a -> EC
ecpow EC_O _ = EC_O
ecpow p x
  | x >= 0            = squareAndMultiply gcompose EC_O p x
  | otherwise         = ecpow invmod (-x)
  where invmod = ginv p

instance CyclicGroup EC where

instance FiniteGroup EC where
    -- TODO: SEA algorithm (Schoof, Elkies and Atkin)
    gcardinality _ = 1
    gorder _ = 1
        
instance AbelianGroup EC where

instance Part3 EC where
    classify EC_O      = 1
    classify (EC _ (ZZP (ZZ y) (ZZ p)) _ _)
      | y <= div p 3     = 1
      | y <= 2 * div p 3 = 2
      | otherwise        = 3

    {-
       Prelude typeclasses
    -}

infixl 6 .+
(.+) :: EC -> EC -> EC
(.+) = gcompose

infixl 7 .*
(.*) :: Integer -> EC -> EC
(.*) = flip gpow


    {-
        Miscellaneous functions
    -}

liftX :: ZZP -> ZZP -> ZZP -> EC
liftX x a b = EC x y a b
    where y2 = x * x * x + a * x + b
          y = head (zzpSqrt y2)
