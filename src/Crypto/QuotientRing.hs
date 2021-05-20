{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Crypto.QuotientRing where

import Crypto.Integers
import Crypto.Ring
import Crypto.Field
import Crypto.Domain

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
data ZnZ = ZnZ { znzElement :: Z, znzIdeal :: Z }
    deriving (Eq)

instance Show ZnZ where
    show (ZnZ a n) = show a ++ " mod " ++ show n

instance Read ZnZ where
    readsPrec _ input =
        let (a, tail1) = head (lex input)
            (txt, tail2) = head (lex tail1)
            (n, tail3) = head (lex tail2)
         in [(ZnZ (read a) (read n), tail3) | txt == "mod"]

instance Ring ZnZ where
    rzero (ZnZ _ n)  = ZnZ (Z 0) n
    radd (ZnZ (Z a) (Z n)) (ZnZ (Z b) (Z n'))
        | n == n'    = ZnZ (Z ((a + b) `mod` n)) (Z n)
        | otherwise  = error "Trying to add two quotient ring elements with different ideals in radd"
    rmul (ZnZ (Z a) (Z n)) (ZnZ (Z b) (Z n'))
        | n == n'    = ZnZ (Z ((a * b) `mod` n)) (Z n)
        | otherwise  = error "Trying to multiply two quotient ring elements with different ideals in rmul"
    rneg (ZnZ (Z a) (Z b)) = ZnZ (Z ((-a) `mod` b)) (Z b)
    rpow                   = znzPow
    rinv                   = znzModinv

instance IdentityRing ZnZ where
    rid (ZnZ _ n) = ZnZ (Z 1) n

instance QuotientRing ZnZ Z where
    qrelement a          = znzElement a
    qrideal a            = znzIdeal a
    -- |Coerce an integer 'a' in 'Z' into 'ZnZ' with ideal 'i in 'Z'.
    -- This also reduces 'a' modulo the ideal 'i'.
    qrcoerce (Z a) (Z i) = ZnZ (Z (a `mod` i)) (Z i)

instance EuclideanDomain ZnZ where
    divide a b = 
        let inverse = rinv b
         in if isNothing inverse
               then error "Dividing a non-unit!"
               else (rmul a (fromJust inverse), rzero a)

instance Field ZnZ where

znzPow :: (Integral a) => ZnZ -> a -> Maybe ZnZ
znzPow b@(ZnZ a n) x
    | x >= 0            = foldl (\acc x -> rmul <$> acc <*> x) (Just identity)
                          (zipWith (\(ZnZ (Z b) _) e -> Just (qrcoerce (Z (b^e)) n :: ZnZ))
                              (modsquares b)
                              (binexpand x))
    | invmod == Nothing = Nothing
    | otherwise         = znzPow new_n (-x)
    where identity = ZnZ (Z 1) n
          invmod = znzModinv b
          modsquares = iterate (\x -> x `rmul` x)
          Just new_n = invmod

znzModinv :: ZnZ -> Maybe ZnZ
znzModinv (ZnZ (Z a) (Z m))
    | g /= 1    = Nothing
    | otherwise = Just (qrcoerce (Z x) (Z m))
    where (g, x, _) = intXgcd a m

-- |Finds the multiplicative order of this element in ZnZ (naive algorithm).
znzOrder :: ZnZ -> Maybe Integer
znzOrder (ZnZ (Z 0) _) = Nothing
znzOrder (ZnZ (Z a) (Z n))
    | gcd a n /= 1 = Nothing
    | otherwise = Just (toInteger (index + 1))
    where Just index = elemIndex (Z 1) (iterate (rmul (Z n)) (Z n))

readZnZx :: String -> Z -> Rx ZnZ
readZnZx input n =
    let Rx a = readZx input
     in Rx $ map (`qrcoerce` n) a


-- |Quotient ring Rx/(m)
data Rxm a = Rxm { rxm_element :: Rx a, rxm_ideal :: Rx a}
    deriving (Read, Show, Eq)

    {-
instance (Show a) => Show (Rxm a) where
    show (Rxm a m) = show a ++ " mod " ++ show m

instance (Read a) => Read (Rxm a) where
    readsPrec i input =
        let ((a, tail1):xs) = readsPrec (i+1) input
            ((txt, tail2):ys) = readsPrec (i+1) tail1
            ((n, tail3):zs) = readsPrec (i+1) tail2
         in if txt == "mod" then [(Rxm a n, tail3)] else []
         -}

instance (EuclideanDomain a) => QuotientRing (Rxm a) (Rx a) where
    qrelement a  = rxm_element a
    qrideal a    = rxm_ideal a
    qrcoerce a n = Rxm (a `modulo` n) n

instance (EuclideanDomain a) => Ring (Rxm a) where
    rzero (Rxm a n)  = Rxm (rzero a) n
    radd (Rxm a n) (Rxm b n')
        | n == n'    = qrcoerce (radd a b) n
        | otherwise  = error "Trying to add two quotient ring elements with different ideals in radd"
    rmul (Rxm a n) (Rxm b n')
        | n == n'    = qrcoerce (rmul a b) n
        | otherwise  = error "Trying to multiply two quotient ring elements with different ideals in rmul"
    rneg (Rxm a n)   = qrcoerce (rneg a) n
        {-
    rpow             = rxmpow
    rinv                   = znzModinv

rx_modmul :: (EuclideanDomain a) => Rx a -> Rx a -> Rx a
rx_modmul a b n =
    let double x = modulo (x `radd` x) n
        identity = rid a
        zero     = rzero a
     in if b == zero
           then identity
           else if even 

rxmpow :: (EuclideanDomain a, Integral b) => Rxm a -> b -> Maybe (Rxm a)
rxmpow b e
    | e == 0            = Just identity
    | e >= 0            = foldl (\acc x -> rmul <$> acc <*> x) (Just identity)
                          (zipWith (\(Rxm b n) e -> Just (qrcoerce (fromJust $ rpow b e) n))
                              (modsquares b)
                              (binexpand e))
    | invmod == Nothing = Nothing
    | otherwise         = rxmpow new_n (-e)
    where identity = rid b
          invmod = rinv b
          Just new_n = invmod


rx_modpow :: (Field a, Integral b) => Rx a -> b -> Rx a -> Rx a
rx_modpow b 0 n = rid b
rx_modpow b e n =
    let square x = rmul x x
     in if even e
           then modulo (square (rx_modpow b (e `div` 2) n)) n
           else modulo (rmul b (rx_modpow b (e - 1) n)) n
           -}

instance (EuclideanDomain a) => IdentityRing (Rxm a) where
    rid (Rxm a n) = Rxm (rid a) n


    {-
        MISCELLANEOUS FUNCTIONS

ntt :: (QuotientRing a) => [a] -> a -> [(a, a)]
ntt a g = let 
    -}

rxmodadd :: (EuclideanDomain a) => Rx a -> Rx a -> Rx a -> Rx a
rxmodadd a b = modulo (radd a b)

rxmodmul :: (EuclideanDomain a) => Rx a -> Rx a -> Rx a -> Rx a
rxmodmul (Rx a) (Rx b) n =
    let a' = zip a [0..]
     in foldl' (\acc (x, i) -> radd acc (scalarmul x (Rx (replicate i (rzero x) ++ b)))) (Rx []) a'
 

modsquares :: (EuclideanDomain a) => Rxm a -> [Rxm a]
modsquares (Rxm x n) = map (`Rxm` n) (iterate (\x -> rxmodmul x x n) x)
