{-# LANGUAGE ScopedTypeVariables #-}
module Crypto.Algebra.Generic where

import Crypto.Algebra.Group.Class
import Crypto.Algebra.Ring.Class
import Crypto.Algebra.Ring.QuotientRing
import Crypto.Algebra.Domain.Class as D
import Crypto.Algebra.Field.Class

import Crypto.Algebra.ZZ
import Crypto.Algebra.ZZN
import Crypto.Algebra.ZZP

import GHC.Real as R

import Data.List
import Data.Maybe

import Debug.Trace

-- |Generates a list of squares given a multiplication function.
squares :: (a -> a -> a) -> a -> [a]
squares mult = iterate (\x -> x `mult` x)

-- |Shanks' Baby-Step Giant-Step discrete logarithm algorithm.
bsgs :: (Integral b, CyclicGroup a, FiniteGroup a, AbelianGroup a, Group a, Eq a) => a -> a -> b -> Maybe Int
bsgs g h ord
  | g == h = Just 1
  | otherwise =
    let 
        n = 1 + (floor . sqrt . fromIntegral) ord
        u = gpow g (-n)
        babystep n = iterate (gcompose n) (gid n)
        giantstep h u = iterate (gcompose u) h
        bstep = take n $ babystep g
        gstep = take n $ giantstep h u
    -- Get the output from this match
    in case bstep `intersect` gstep of
        [] -> Nothing
        (match:_) -> R.mod <$> val <*> Just (fromIntegral ord)
            where getInd = elemIndex match
                  val = (+) <$> getInd bstep <*> ((n*) <$> getInd gstep)

-- |Solver for the Chinese Remainder Theorem, given a list of quotient ring elements.
crt :: forall a b . (Ring a, Ring b, (QuotientRing a b)) => [a] -> Maybe a
crt []         = Nothing
crt [aa]       = Just aa
crt (aa:bb:cs) =
    let 
        p = qrelement aa
        q = qrideal aa
        r = qrelement bb
        s = qrideal bb
        t = qrcoerce r s :: a
        u = qrcoerce p s :: a
        v = qrcoerce q s :: a
        y = rmul <$> Just (t `radd` rneg u) <*> rinv v
    in case y of
        Nothing          -> Nothing
        Just ans -> crt (qrcoerce (p `radd` (q `rmul` qrelement ans)) (q `rmul` s) : cs)

-- | Given a list of residues and a list of moduli, get the CRT
crtList :: [Integer] -> [Integer] -> Maybe ZZN
crtList a b = crt (zipWith (\x y -> ZZN (ZZ x) (ZZ y)) a b)

-- |Algorithm to reduce discrete logarithm for an element with prime power order.
logReduce :: (CyclicGroup a, FiniteGroup a, AbelianGroup a, Group a, Eq a, Integral b, Integral c) => a -> a -> b -> c -> Maybe Int
logReduce g h q e =
    let
        -- (g^q^(exp-1)^x_n = (h*g^(-x))^(exp-1-n)
        -- (m^x_n = (h * g^(-x)) ^ (exp-1-n)
        pow_pow a b = gpow a (q ^ b)
        negpow x    = gpow g (-x)
        m           = pow_pow g (e-1)

        x_n 0 = do 
            let b = pow_pow h (e-1)
            bsgs m b q
        x_n n = do
            prevx <- x_n (n-1)
            let rterm = negpow prevx
                b     = pow_pow (h `gcompose` rterm) (e-1-n)
            xn <- bsgs m b q
            return $ prevx + xn * fromIntegral q ^ n
    in
        x_n (e - 1)


-- |Calculates the discrete logarithm g^x = h, where 'factors' are
-- the factors of g's order within its group.
pohligHellman :: (CyclicGroup a, FiniteGroup a, AbelianGroup a, Group a, Eq a) => a -> a -> [(Integer, Integer)] -> Maybe Int
pohligHellman g h factors = if isNothing res
                               then Nothing
                               else return (fromIntegral . toInteger . qrelement $ fromJust res)
    where
        ord                    = foldr (\x acc -> acc * uncurry (^) x) 1 factors
        tmp t (q,e)            = gpow t (ord `R.div` (q^e))
        subproblem a b x@(q,e) = fromJust $ logReduce (tmp a x) (tmp b x) q e
        -- Can't use crtList because this needs a type annotation
        zzps                   = zipWith qrcoerce (map (ZZ . toInteger . subproblem g h) factors)
                                                  (map (ZZ . uncurry (^)) factors) :: [ZZP]
        res = crt zzps
