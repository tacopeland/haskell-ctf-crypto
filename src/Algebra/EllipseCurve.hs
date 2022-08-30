-- |Made to solve CryptoHack's Ellipse Curve Cryptography challenge.
module Algebra.EllipseCurve (EllipseCurve(..)) where

import Internal.Helpers

import Algebra.Structure.Group
import Algebra.Structure.Ring

import Algebra.ZZ
import Algebra.ZZP

data EllipseCurve = EllipseCurve { elcX :: ZZP, elcY :: ZZP, elcD :: ZZP }
    deriving (Show,Eq)

instance Group EllipseCurve where
    gcompose (EllipseCurve px py d) (EllipseCurve qx qy _) = EllipseCurve (px * qx + d * py * qy) (px * qy + py * qx) d
    gpow p x
      | x < 0 = gpow (ginv p) (-x)
      | otherwise = squareAndMultiply gcompose (gid p) p x
    ginv _ = error "Not implemented"
    gid (EllipseCurve _ _ d) = EllipseCurve (qrcoerce (ZZ 1) (qrideal d)) (qrcoerce (ZZ 0) (qrideal d)) d

instance CyclicGroup EllipseCurve where

instance FiniteGroup EllipseCurve where
    gcardinality = gorder
    gorder _ = error "Not implemented"

instance AbelianGroup EllipseCurve


instance Part3 EllipseCurve where
    classify (EllipseCurve _ (ZZP (ZZ y) (ZZ p)) _)
      | y <= div p 3     = 1
      | y <= 2 * div p 3 = 2
      | otherwise        = 3
