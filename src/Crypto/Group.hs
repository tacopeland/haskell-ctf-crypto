module Crypto.Group where

import Crypto.Integers

class Group a where
    gcompose :: a -> a -> a
    gpow :: (Integral i) => a -> i -> a
    ginv :: a -> a
    gid :: a -> a

data ZmodP = ZmodP Integer Integer
    deriving (Show, Eq)

-- Multiplicative group
instance Group ZmodP where
    gcompose (ZmodP a p) (ZmodP b p')
        | p == p'   = ZmodP ((a * b) `mod` p) p
        | otherwise = error "Conflicting moduli in gcompose"
    gpow = zmodp_pow
    ginv = zmodp_modinv
    gid (ZmodP _ p) = ZmodP 1 p

zmodp_pow :: (Integral a) => ZmodP -> a -> ZmodP
zmodp_pow n x
    | x >= 0              = foldr gcompose (gid n)
                            (zipWith (\(ZmodP a p) e -> ZmodP ((a^e) `mod` p) p)
                                (modsquares n)
                                (binexpand x))
    | otherwise           = gpow new_n (-x)
    where new_n = zmodp_modinv n
          modsquares n = iterate (\x -> x `gcompose` x) n
          binexpand 0 = []
          binexpand a 
            | a `mod` 2 == 1 = 1 : leftover
            | otherwise      = 0 : leftover
            where leftover = binexpand $ a `div` 2

zmodp_modinv :: ZmodP -> ZmodP
zmodp_modinv (ZmodP a p) = ZmodP x p
    where (_, x, _) = xgcd a p


data ZmodN = ZmodN Integer Integer
    deriving (Show, Eq)

-- Additive group
instance Group ZmodN where
    gcompose (ZmodN a n) (ZmodN b n')
        | n == n'   = ZmodN ((a + b) `mod` n) n
        | otherwise = error "Conflicting moduli in gcompose"
    gpow (ZmodN a n) e = (ZmodN ((a * (fromIntegral e)) `mod` n) n)
    ginv (ZmodN a n) = (ZmodN ((-a) `mod` n) n)
    gid (ZmodN _ p) = ZmodN 1 p
