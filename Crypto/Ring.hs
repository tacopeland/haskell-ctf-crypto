module Crypto.Ring where

import Crypto.Group
import Crypto.Integers

import Data.List

class Ring a where
    radd :: a -> a -> a
    rsub :: a -> a -> a
    rmul :: a -> a -> a
    rpow :: (Integral i) => a -> i -> Maybe a
    rinv :: a -> Maybe a


-- For ring and multiplicative group
data ZnZ = ZnZ Integer Integer
    deriving (Show, Eq)

instance Ring ZnZ where
    radd (ZnZ a n) (ZnZ b n')
        | n == n'   = ZnZ ((a + b) `mod` n) n
        | otherwise = error "Conflicting moduli in radd"
    rsub a (ZnZ b n) = radd a (ZnZ (n - b) n)
    rmul (ZnZ a n) (ZnZ b n')
        | n == n'   = ZnZ ((a * b) `mod` n) n
        | otherwise = error "Conflicting moduli in rmul"
    rpow               = znz_pow
    rinv               = znz_modinv


znz_pow :: (Integral a) => ZnZ -> a -> Maybe ZnZ
znz_pow n x
    | x >= 0              = Just $ foldr rmul identity
                            (zipWith (\(ZnZ a mod) e -> ofIntegral (a^e) mod)
                                (modsquares n)
                                (binexpand x))
    | invmod == Nothing   = Nothing
    | otherwise           = znz_pow new_n (-x)
    where identity = ZnZ 1 (modulus n)
          invmod = znz_modinv n
          Just new_n = invmod
          modsquares n = iterate (\x -> x `rmul` x) n
          binexpand 0 = []
          binexpand a 
            | a `mod` 2 == 1 = 1 : leftover
            | otherwise      = 0 : leftover
            where leftover = binexpand $ a `div` 2

znz_asInteger :: ZnZ -> Integer
znz_asInteger (ZnZ a n) = a

modulus :: ZnZ -> Integer
modulus (ZnZ a n) = n

ofIntegral :: (Integral a, Integral b) => a -> b -> ZnZ
ofIntegral a n = ZnZ (toInteger a `mod` toInteger n) (toInteger n)

znz_modinv :: ZnZ -> Maybe ZnZ
znz_modinv (ZnZ a m)
    | g /= 1    = Nothing
    | otherwise = Just (ofIntegral x m)
    where (g, x, _) = xgcd a m

-- |Finds the multiplicative order of this element in ZnZ
znz_order :: ZnZ -> Maybe Integer
znz_order (ZnZ 0 _) = Nothing
znz_order n
    | gcd (znz_bareInteger n) (modulus n) /= 1 = Nothing
    | otherwise = Just (toInteger (index + 1))
    where Just index = elemIndex identity (iterate (rmul n) n)
          identity = ZnZ 1 (modulus n)
