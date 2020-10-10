module Crypto.Ring where

import Crypto.Group
import Crypto.Integers

class Ring a where
    radd :: a -> a -> a
    rsub :: a -> a -> a
    rmul :: a -> a -> a
    rpow :: (Integral i) => a -> i -> Maybe a
    rinv :: a -> Maybe a


data ZmodN = ZmodN Integer Integer
    deriving (Show, Eq)


-- Only the multiplicative group
instance Group ZmodN where
    gcompose (ZmodN a n) (ZmodN b n')
        | n == n'   = ZmodN ((a * b) `mod` n) n
        | otherwise = error "Conflicting moduli in gcompose"
    gpow = pow
    ginv = modinv


instance Ring ZmodN where
    radd (ZmodN a n) (ZmodN b n')
        | n == n'   = ZmodN ((a + b) `mod` n) n
        | otherwise = error "Conflicting moduli in radd"
    rsub a (ZmodN b n) = radd a (ZmodN (n - b) n)
    rmul (ZmodN a n) (ZmodN b n')
        | n == n'   = ZmodN ((a * b) `mod` n) n
        | otherwise = error "Conflicting moduli in rmul"
    rpow               = pow
    rinv               = modinv


pow :: (Integral a) => ZmodN -> a -> Maybe ZmodN
pow n x
    | x >= 0              = Just $ foldr rmul identity
                            (zipWith (\(ZmodN a mod) e -> ofIntegral (a^e) mod)
                                (modsquares n)
                                (binexpand x))
    | invmod == Nothing   = Nothing
    | otherwise           = pow new_n (-x)
    where identity = ZmodN 1 (modulus n)
          invmod = modinv n
          Just new_n = invmod
          modsquares n = iterate (\x -> x `rmul` x) n
          binexpand 0 = []
          binexpand a 
            | a `mod` 2 == 1 = 1 : leftover
            | otherwise      = 0 : leftover
            where leftover = binexpand $ a `div` 2

bareInteger :: ZmodN -> Integer
bareInteger (ZmodN a n) = a

modulus :: ZmodN -> Integer
modulus (ZmodN a n) = n

ofIntegral :: (Integral a, Integral b) => a -> b -> ZmodN
ofIntegral a n = ZmodN (toInteger a `mod` toInteger n) (toInteger n)

modinv :: ZmodN -> Maybe ZmodN
modinv (ZmodN a m)
    | g /= 1    = Nothing
    | otherwise = Just (ofIntegral x m)
    where (g, x, _) = xgcd a m
