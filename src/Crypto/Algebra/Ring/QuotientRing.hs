{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Crypto.Algebra.Ring.QuotientRing where

import Crypto.Integers
import Crypto.Algebra.Group.Class
import Crypto.Algebra.Ring.Class
import Crypto.Algebra.Field.Class
import Crypto.Algebra.Domain.Class

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

    {-
-- |Horner's rule
evaluatePoly (Rx a) x =
    foldl' (\acc c -> radd (rmul acc x) c) (rzero (head a)) (reverse a)

instance (EuclideanDomain a) => IdentityRing (Rxm a) where
    rid (Rxm a n) = Rxm (rid a) n


-- |The elliptic curve group over GF(p)
data EC = EC_O | EC { ec_x :: ZZP, ec_y :: ZZP, ec_A :: ZZP, ec_B :: ZZP }
    deriving (Show, Eq)

instance Group EC where
    gcompose p@(EC x1 y1 a b) q@(EC x2 y2 a' b')
      | a /= a' || b /= b' = error "Adding points on two different elliptic curves."
      | x1 == x2 && y1 == rneg y2 = EC_O
      | otherwise =
          let two = radd (rid x1) (rid x1)
              three = radd (rid x1) (radd (rid x1) (rid x1))
              lambda = if p /= q
                          then rmul (radd y2 (rneg y1)) (fromJust $ rinv (radd x2 (rneg x1)))
                          else rmul (radd (rmul three (rmul x1 x1)) a) (fromJust $ rinv (rmul two y1))
              x3 = radd (rmul lambda lambda) (rneg (radd x1 x2))
              y3 = radd (rmul lambda (radd x1 (rneg x3))) (rneg y1)
           in EC x3 y3 a b
    gcompose EC_O q = q
    gcompose p EC_O = p
    -- TODO
    gpow = ecpow
    ginv EC_O = EC_O
    ginv (EC x y a b) = EC x (rneg y) a b
    gid _ = EC_O

ecpow :: (Integral a) => EC -> a -> EC
ecpow EC_O _ = EC_O
ecpow p e
  | e == 0 = EC_O
  | e == 1 = p
  | e >  1 = foldl gcompose identity (zipWith ecpow modsquares (binexpand e))
  | otherwise         = ecpow invmod (-e)
  where identity = EC_O
        invmod = ginv p
        modsquares = iterate (\x -> gcompose x x) p

instance CyclicGroup EC where

instance FiniteGroup EC where
    gorder (EC (ZnZ _ (Z n)) _ _ _) = n
    gorder EC_O = 0
        
instance AbelianGroup EC where

    {-
        MISCELLANEOUS FUNCTIONS
    -}

{- (This doesn't work yet, need factorization routines)
isPrimitiveRoot :: ZnZ -> Bool 
isPrimitiveRoot x@(ZnZ (Z a) (Z n)) =
    coprime a n && rpow x (n - 1) == Just (rid x)
    -}

isqrt = toInteger . floor . sqrt . fromIntegral

isPrime k = k > 1 && null [ x | x <- [2..isqrt k], k `mod` x == 0]

ntt' :: (IdentityRing a) => [a] -> a -> [a]
ntt' a' g =
    let m = ceiling (logBase 2 (fromIntegral $ length a'))
        n = 2 ^ m
        a = a' ++ replicate (n - length a') (rzero g)
        (a0, a1) = foldr (\x (xs,ys) -> (x:ys,xs)) ([],[]) a
        Just inv2 = rinv (radd (rid g) (rid g))
        y0 = ntt' a0 (rmul inv2 g)
        y1 = ntt' a1 (rmul inv2 g)
        omegas = iterate (rmul g) (rid g)
        omegay1 = zipWith rmul y1 omegas
        ys = zipWith (\x y -> (radd x y, radd x (rneg y))) y0 omegay1
        y = uncurry (++) (unzip ys)
     in if length a' == 1 then a' else y

ntt a' g =
    let a = a' ++ replicate (2 * length a') (rzero g)
     in ntt' a g

invntt' :: (Integral a) => [ZnZ] -> ZnZ -> a -> [ZnZ]
invntt' y' g l =
    let m = ceiling (logBase 2 (fromIntegral $ length y'))
        n = 2 ^ m
        y = y' ++ replicate (n - length y') (rzero g)
        (y0, y1) = foldr (\x (xs,ys) -> (x:ys,xs)) ([],[]) y
        a0 = invntt' y0 g (l + 1)
        a1 = invntt' y1 g (l + 1)
        omegas = map (fromJust . rinv) (drop (n `div` 2) (iterate (rmul g) (rid g)))
        Just i = rpow (radd (rid g) (rid g)) l >>= rinv
        as = zipWith3 (\x y omega -> (rmul i (radd x y), rmul i (rmul omega (radd x (rneg y))))) a0 a1 omegas
        a = uncurry (++) (unzip as)
     in if length y' == 1 then y' else a

invntt y g =
    let Just ginv = rinv g
        Just inv = rinv (ZnZ (Z (toInteger $ length y)) (qrideal g))
     in map (rmul inv) (invntt' y ginv 1)

rxmodadd :: (EuclideanDomain a) => Rx a -> Rx a -> Rx a -> Rx a
rxmodadd a b = modulo (radd a b)

rxmodmul :: (EuclideanDomain a) => Rx a -> Rx a -> Rx a -> Rx a
rxmodmul (Rx a) (Rx b) n =
    let a' = zip a [0..]
     in foldl' (\acc (x, i) -> radd acc (scalarmul x (Rx (replicate i (rzero x) ++ b)))) (Rx []) a'
 



-}
