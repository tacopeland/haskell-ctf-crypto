{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Crypto.QuotientRing where

import Crypto.Ring
import Crypto.Domain
import Crypto.Integers

import Data.List
import Data.Maybe


    {-
        TYPECLASS DEFINITIONS
    -}

class (Ring a, Ring b) => QuotientRing a b | a -> b where
    qrelement :: a -> b
    qrideal :: a -> b
    qrcoerce :: b -> b -> a


    {-
        DATA TYPES AND INSTANCE DEFINITIONS
    -}

-- |Quotient ring Z/nZ
data ZnZ = ZnZ { znz_element :: Z, znz_ideal :: Z }
    deriving (Eq)

instance Show ZnZ where
    show (ZnZ a n) = show a ++ " mod " ++ show n

instance Read ZnZ where
    readsPrec _ input =
        let (a, tail1) = head (lex input)
            (txt, tail2) = head (lex tail1)
            (n, tail3) = head (lex tail2)
         in if txt == "mod" then [(ZnZ (read a) (read n), tail3)] else []

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

instance IdentityRing ZnZ where
    rid (ZnZ _ n) = ZnZ (Z 1) n

instance QuotientRing ZnZ Z where
    qrelement a          = znz_element a
    qrideal a            = znz_ideal a
    -- |Coerce an integer 'a' in 'Z' into 'ZnZ' with ideal 'i in 'Z'.
    -- This also reduces 'a' modulo the ideal 'i'.
    qrcoerce (Z a) (Z i) = ZnZ (Z (a `mod` i)) (Z i)

znz_pow :: (Integral a) => ZnZ -> a -> Maybe ZnZ
znz_pow b@(ZnZ a n) x
    | x >= 0              = foldl (\acc x -> rmul <$> acc <*> x) (Just identity)
                            (zipWith (\(ZnZ (Z b) _) e -> Just (qrcoerce (Z (b^e)) n :: ZnZ))
                                (modsquares b)
                                (binexpand x))
    | invmod == Nothing   = Nothing
    | otherwise           = znz_pow new_n (-x)
    where identity = ZnZ (Z 1) n
          modsquares = iterate (\x -> x `rmul` x)
          binexpand 0 = []
          binexpand a 
              | even a    = 0 : leftover
              | otherwise = 1 : leftover
              where leftover = binexpand $ a `div` 2
          invmod = znz_modinv b
          Just new_n = invmod

znz_modinv :: ZnZ -> Maybe ZnZ
znz_modinv (ZnZ a m)
    | g /= rid a = Nothing
    | otherwise  = Just (qrcoerce x m)
    where (g, x, _) = xgcd a m

-- |Finds the multiplicative order of this element in ZnZ (naive algorithm).
znz_order :: ZnZ -> Maybe Integer
znz_order (ZnZ (Z 0) _) = Nothing
znz_order (ZnZ (Z a) (Z n))
    | gcd a n /= 1 = Nothing
    | otherwise = Just (toInteger (index + 1))
    where Just index = elemIndex (Z 1) (iterate (rmul (Z n)) (Z n))


-- |Quotient ring Rx/(m)
data Rxm a = Rxm { rxm_element :: Rx a, rxm_ideal :: Rx a}
    deriving (Eq)

instance (Show a) => Show (Rxm a) where
    show (Rxm a m) = show a ++ " mod " ++ show m

instance (Read a) => Read (Rxm a) where
    readsPrec i input =
        let ((a, tail1):xs) = readsPrec (i+1) input
            ((txt, tail2):ys) = readsPrec (i+1) tail1
            ((n, tail3):zs) = readsPrec (i+1) tail2
         in if txt == "mod" then [(Rxm a n, tail3)] else []


