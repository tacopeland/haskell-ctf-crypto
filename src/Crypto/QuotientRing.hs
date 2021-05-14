{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Crypto.QuotientRing where

import Crypto.Ring
import Crypto.Domain
import Crypto.Integers

import Data.List
import Data.Maybe


class (Ring a, Ring b) => QuotientRing a b | a -> b where
    qrelement :: a -> b
    qrideal :: a -> b
    qrcoerce :: b -> b -> a


-- Z/nZ
data ZnZ = ZnZ { element :: Z, ideal :: Z }
    deriving (Show, Eq)

instance Ring ZnZ where
    rzero (ZnZ _ n)  = ZnZ (Z 0) n
    radd (ZnZ (Z a) (Z n)) (ZnZ (Z b) (Z n'))
        | n == n'    = ZnZ (Z ((a + b) `mod` n)) (Z n)
        | otherwise  = error "Trying to add two quotient ring elements with different ideals in radd"
    rmul (ZnZ (Z a) (Z n)) (ZnZ (Z b) (Z n'))
        | n == n'    = ZnZ (Z ((a * b) `mod` n)) (Z n)
        | otherwise  = error "Trying to multiply two quotient ring elements with different ideals in rmul"
    rneg (ZnZ (Z a) (Z b)) = ZnZ (Z ((-a) `mod` b)) (Z b)
    rpow                   = znz_pow
    rinv                   = znz_modinv

instance QuotientRing ZnZ Z where
    qrelement a          = element a
    qrideal a            = ideal a
    -- |Coerce an integer 'a' in 'Z' into 'ZnZ' with ideal 'i in 'Z'.
    -- This also reduces 'a' modulo the ideal 'i'.
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
znz_modinv (ZnZ a m)
    | g /= rid a = Nothing
    | otherwise  = Just (qrcoerce x m)
    where (g, x, _) = xgcd a m

-- |Finds the multiplicative order of this element in ZnZ
znz_order :: ZnZ -> Maybe Integer
znz_order (ZnZ (Z 0) _) = Nothing
znz_order (ZnZ (Z a) (Z n))
    | gcd a n /= 1 = Nothing
    | otherwise = Just (toInteger (index + 1))
    where Just index = elemIndex (Z 1) (iterate (rmul (Z n)) (Z n))
