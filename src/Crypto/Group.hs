module Crypto.Group where

import Data.List
import Data.Maybe

import Crypto.Integers


    {-
        TYPECLASS DEFINITIONS
    -}

class Group a where
    gcompose :: a -> a -> a
    gpow :: (Integral i) => a -> i -> a
    ginv :: a -> a
    gid :: a -> a

class FiniteGroup a where
    gorder :: a -> Integer

class AbelianGroup a where

class CyclicGroup a where


    {-
        DATA TYPES AND INSTANCE DEFINITIONS
    -}

-- |The multiplicative group of ZmodP
data ZmodP = ZmodP Integer Integer
    deriving (Eq)

instance Show ZmodP where
    show (ZmodP a p) = show a ++ " mod " ++ show p

instance Read ZmodP where
    readsPrec _ input =
        let (a, tail1) = head (lex input)
            (txt, tail2) = head (lex tail1)
            (n, tail3) = head (lex tail2)
         in if txt == "mod" then [(ZmodP (read a) (read n), tail3)] else []

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
            | even a       = 0 : leftover
            | otherwise    = 1 : leftover
            where leftover = binexpand $ a `div` 2

zmodp_modinv :: ZmodP -> ZmodP
zmodp_modinv (ZmodP a p) = ZmodP (x `mod` p) p
    where (_, x, _) = int_xgcd a p

instance FiniteGroup ZmodP where
    -- NAIVE
    gorder n = (toInteger $ fromJust $ elemIndex (gid n) (iterate (gcompose n) n)) + 1

instance AbelianGroup ZmodP where

instance CyclicGroup ZmodP where



-- |The additive group of ZmodN
data ZmodN = ZmodN Integer Integer
    deriving (Eq)

instance Show ZmodN where
    show (ZmodN a n) = show a ++ " mod " ++ show n

instance Read ZmodN where
    readsPrec _ input =
        let (a, tail1) = head (lex input)
            (txt, tail2) = head (lex tail1)
            (n, tail3) = head (lex tail2)
         in if txt == "mod" then [(ZmodN (read a) (read n), tail3)] else []

instance Group ZmodN where
    gcompose (ZmodN a n) (ZmodN b n')
        | n == n'   = ZmodN ((a + b) `mod` n) n
        | otherwise = error "Conflicting moduli in gcompose"
    gpow (ZmodN a n) e = (ZmodN ((a * (fromIntegral e)) `mod` n) n)
    ginv (ZmodN a n) = (ZmodN ((-a) `mod` n) n)
    gid (ZmodN _ p) = ZmodN 0 p

instance FiniteGroup ZmodN where
    gorder n = (toInteger $ fromJust $ elemIndex (gid n) (iterate (gcompose n) n)) + 1

instance AbelianGroup ZmodN where

instance CyclicGroup ZmodN where
