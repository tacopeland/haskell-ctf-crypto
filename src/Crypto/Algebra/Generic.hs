{-# LANGUAGE ScopedTypeVariables #-}
module Crypto.Algebra.Generic where

import Crypto.Integers as Int
import Crypto.Helpers

import Crypto.Algebra.Group.Class
import Crypto.Algebra.Ring.Class
import Crypto.Algebra.Ring.QuotientRing
import Crypto.Algebra.Domain.Class as D
import Crypto.Algebra.Field.Class

import Crypto.Algebra.ZZ
import Crypto.Algebra.ZZN
import Crypto.Algebra.ZZP
import Crypto.Algebra.EC
import Crypto.Algebra.Factor

import GHC.Real as R

import Control.Parallel.Strategies
import Data.List
import Data.Maybe
import Debug.Trace
import Math.NumberTheory.Roots

-- |Generates a list of squares given a multiplication function.
squares :: (a -> a -> a) -> a -> [a]
squares mult = iterate (\x -> x `mult` x)

-- |Brute-force the discrete logarithm g^e == h.
discreteLogBrute g h x i n
  | i > n = -1
  | otherwise = 
    let newX = gcompose g x
     in if newX == h
         then i
         else discreteLogBrute g h newX (i + 1) n

-- |Shanks' Baby-Step Giant-Step discrete logarithm algorithm.
bsgs :: (Integral b, CyclicGroup a, FiniteGroup a, AbelianGroup a, Group a, Eq a) => a -> a -> b -> Maybe Integer
bsgs g h ord
  | g == h = Just 1
  | b /= (-1) = Just b
  | otherwise =
    let 
        n = 1 + (integerSquareRoot . toInteger) ord
        u = gpow g (-n)
        babystep n = iterate (gcompose n) (gid n)
        giantstep h u = iterate (gcompose u) h
        bstep = genericTake n $ babystep g
        gstep = genericTake n $ giantstep h u
    -- Get the output from this match
    in case collide bstep gstep [] 0 of
         Nothing            -> Nothing
         Just (match, i, j) -> Just (R.mod val (fromIntegral ord))
            where val = toInteger i + n * toInteger j
  where 
      -- Try the first 50 powers of g before moving onto BSGS
      b = discreteLogBrute g h g 2 50

-- |Pollard's Rho algorithm for use in solving the discrete logarithm problem.
pollardRhoDLog :: (Part3 a, FiniteGroup a, AbelianGroup a, Group a, Eq a) => a -> a -> Integer -> Maybe Integer
pollardRhoDLog g h ord
  | g == h = Just 1
  | b /= (-1) = Just b
  | otherwise =
    let mapA x n
          | classify x == 1 = (n + 1) `mod` ord
          | classify x == 2 = (n * 2) `mod` ord
          | classify x == 3 = n
          | otherwise = error "Invalid classification function!"
        mapB x n
          | classify x == 1 = n
          | classify x == 2 = (n * 2) `mod` ord
          | classify x == 3 = (n + 1) `mod` ord
          | otherwise = error "Invalid classification function!"
        mix x
          | classify x == 1 = g `gcompose` x
          | classify x == 2 = x `gcompose` x
          | classify x == 3 = h `gcompose` x
          | otherwise = error "Invalid classification function!"
        x0 = gid g
        inner x a b x' a' b' =
            let xi = mix x
                ai = mapA x a
                bi = mapB x b
                x2i' = mix x'
                x2i = mix x2i'
                a2i = mapA x2i' (mapA x' a')
                b2i = mapB x2i' (mapB x' b')
                u = (ai - a2i) `mod` ord
                v = (b2i - bi) `mod` ord
                (d, s, _) = Int.xgcd v ord
                w = (s * u) `mod` ord
                newOrd = ord `div` d
             in if xi == x2i
                   then filter (\x -> gpow g x == h) (map (\k -> (w `div` d) + k * newOrd) [0..(d - 1)])
                   else inner xi ai bi x2i a2i b2i
        res = inner x0 0 0 x0 0 0 --[]
     in if null res
           then Nothing
           else Just (head res)
     where b = discreteLogBrute g h g 2 50



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
crtList a b = crt (zipWith (\x y -> qrcoerce (ZZ x) (ZZ y)) a b)

-- |Algorithm to reduce discrete logarithm for an element with prime power order.
logReduce :: (Part3 a, CyclicGroup a, FiniteGroup a, AbelianGroup a, Group a, Eq a) => a -> a -> Integer -> Integer -> Maybe Integer
logReduce g h q e =
    let
        -- (g^q^(exp-1)^x_n = (h*g^(-x))^(exp-1-n)
        -- (m^x_n = (h * g^(-x)) ^ (exp-1-n)
        pow_pow a b = gpow a (q ^ b)
        negpow x    = gpow g (-x)
        m           = pow_pow g (e-1)

        x_n 0 = do 
            let b = pow_pow h (e-1)
            --pollardRhoDLog m b q classifyEC
            pollardRhoDLog m b q
        x_n n = do
            prevx <- x_n (n-1)
            let rterm = negpow prevx
                b     = pow_pow (h `gcompose` rterm) (e-1-n)
            --xn <- pollardRhoDLog m b q classifyEC
            xn <- pollardRhoDLog m b q
            return $ prevx + xn * fromIntegral q ^ n
    in
        x_n (e - 1)


-- |Calculates the discrete logarithm g^x = h, where 'factors' are
-- the factors of g's order within its group.
pohligHellman :: (Part3 a, CyclicGroup a, FiniteGroup a, AbelianGroup a, Group a, Eq a) => a -> a -> [(Integer, Integer)] -> Maybe Integer
pohligHellman g h factors = if isNothing res
                               then Nothing
                               else return (fromIntegral . toInteger . qrelement $ fromJust res)
    where
        ord                    = foldr (\x acc -> acc * uncurry (^) x) 1 factors
        tmp t (q,e)            = gpow t (ord `R.div` (q^e))
        subproblem a b x@(q,e) = fromJust $ logReduce (tmp a x) (tmp b x) q e
        res                    = crtList (parMap rdeepseq (toInteger . subproblem g h) factors)
                                         (map (uncurry (^)) factors)

discreteLog :: (Part3 a, CyclicGroup a, FiniteGroup a, AbelianGroup a, Group a, Eq a) => a -> a -> [(Integer, Integer)] -> Maybe Integer
discreteLog g h factors
  | g == h = Just 1
  | b /= -1 = Just b
  |otherwise = pohligHellman g h factors
    where b = discreteLogBrute g h g 2 50
