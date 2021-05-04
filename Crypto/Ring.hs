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


data ZmodN = ZmodN Integer Integer
    deriving (Show, Eq)


-- Only the multiplicative group
instance Group ZmodN where
    gcompose (ZmodN a n) (ZmodN b n')
        | n == n'   = ZmodN ((a * b) `mod` n) n
        | otherwise = error "Conflicting moduli in gcompose"
    gpow = zmodn_pow
    ginv = zmodn_modinv
    gorder = zmodn_order
    gid n = ZmodN 1 (modulus n)


instance Ring ZmodN where
    radd (ZmodN a n) (ZmodN b n')
        | n == n'   = ZmodN ((a + b) `mod` n) n
        | otherwise = error "Conflicting moduli in radd"
    rsub a (ZmodN b n) = radd a (ZmodN (n - b) n)
    rmul (ZmodN a n) (ZmodN b n')
        | n == n'   = ZmodN ((a * b) `mod` n) n
        | otherwise = error "Conflicting moduli in rmul"
    rpow               = zmodn_pow
    rinv               = zmodn_modinv


zmodn_pow :: (Integral a) => ZmodN -> a -> Maybe ZmodN
zmodn_pow n x
    | x >= 0              = Just $ foldr rmul identity
                            (zipWith (\(ZmodN a mod) e -> ofIntegral (a^e) mod)
                                (modsquares n)
                                (binexpand x))
    | invmod == Nothing   = Nothing
    | otherwise           = zmodn_pow new_n (-x)
    where identity = ZmodN 1 (modulus n)
          invmod = zmodn_modinv n
          Just new_n = invmod
          modsquares n = iterate (\x -> x `rmul` x) n
          binexpand 0 = []
          binexpand a 
            | a `mod` 2 == 1 = 1 : leftover
            | otherwise      = 0 : leftover
            where leftover = binexpand $ a `div` 2

zmodn_bareInteger :: ZmodN -> Integer
zmodn_bareInteger (ZmodN a n) = a

modulus :: ZmodN -> Integer
modulus (ZmodN a n) = n

ofIntegral :: (Integral a, Integral b) => a -> b -> ZmodN
ofIntegral a n = ZmodN (toInteger a `mod` toInteger n) (toInteger n)

zmodn_modinv :: ZmodN -> Maybe ZmodN
zmodn_modinv (ZmodN a m)
    | g /= 1    = Nothing
    | otherwise = Just (ofIntegral x m)
    where (g, x, _) = xgcd a m

-- |Finds the multiplicative order of this element in ZmodN
zmodn_order :: ZmodN -> Maybe Integer
zmodn_order (ZmodN 0 _) = Nothing
zmodn_order n
    | gcd (zmodn_bareInteger n) (modulus n) /= 1 = Nothing
    | otherwise = Just (toInteger (index + 1))
    where Just index = elemIndex identity (iterate (gcompose n) n)
          identity = ZmodN 1 (modulus n)


