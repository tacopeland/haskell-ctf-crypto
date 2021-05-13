{-# LANGUAGE FlexibleInstances #-}
module Crypto.QuotientRing where

import Crypto.Ring
import Crypto.Integers

import Data.List
import Data.Maybe


class Ring a => QuotientRing a where
    qrelement :: a -> Z
    qrideal :: a -> Z
    qrcoerce :: Z -> Z -> a


-- Z/nZ
data ZnZ = ZnZ Z Z
    deriving (Show, Eq)

instance Ring ZnZ where
    radd (ZnZ (Z a) (Z n)) (ZnZ (Z b) (Z n'))
        | n == n'    = ZnZ (Z ((a + b) `mod` n)) (Z n)
        | otherwise  = error "Trying to add two quotient ring elements with different ideals in radd"
    rmul (ZnZ (Z a) (Z n)) (ZnZ (Z b) (Z n'))
        | n == n'    = ZnZ (Z ((a * b) `mod` n)) (Z n)
        | otherwise  = error "Trying to multiply two quotient ring elements with different ideals in rmul"
    rneg (ZnZ (Z a) (Z b)) = ZnZ (Z ((-a) `mod` b)) (Z b)
    rpow                   = znz_pow
    rinv                   = znz_modinv

instance QuotientRing ZnZ where
    qrelement (ZnZ x _)  = x
    qrideal (ZnZ _ i)    = i
    -- Coercing into a quotient ring is how we take an element modulo an ideal
    qrcoerce (Z a) (Z i) = ZnZ (Z (a `mod` i)) (Z i)


znz_pow :: (Integral a) => ZnZ -> a -> Maybe ZnZ
znz_pow b@(ZnZ (Z a) n) x
    | x >= 0              = foldr (\x y -> rmul <$> x <*> y) (Just identity)
                            (zipWith rpow (modsquares b) (binexpand x))
    | invmod == Nothing   = Nothing
    | otherwise           = znz_pow new_n (-x)
    where identity = ZnZ (Z 1) n
          invmod = znz_modinv b
          -- Rewrite this to handle Nothing invmods
          Just new_n = invmod
          modsquares = iterate (\x -> x `rmul` x)
          binexpand 0 = []
          binexpand a 
            | even a    = 1 : leftover
            | otherwise = 0 : leftover
            where leftover = binexpand $ a `div` 2

znz_modinv :: ZnZ -> Maybe ZnZ
znz_modinv (ZnZ (Z a) (Z m))
    | g /= 1    = Nothing
    | otherwise = Just (qrcoerce (Z x) (Z m))
    where (g, x, _) = xgcd a m

-- |Finds the multiplicative order of this element in ZnZ
znz_order :: ZnZ -> Maybe Integer
znz_order (ZnZ (Z 0) _) = Nothing
znz_order (ZnZ (Z a) (Z n))
    | gcd a n /= 1 = Nothing
    | otherwise = Just (toInteger (index + 1))
    where Just index = elemIndex (Z 1) (iterate (rmul (Z n)) (Z n))
