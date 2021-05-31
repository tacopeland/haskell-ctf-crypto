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
import Crypto.Algebra.Factor

import GHC.Real as R

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
    in case bstep `intersect` gstep of
        [] -> Nothing
        (match:_) -> R.mod <$> val <*> Just (fromIntegral ord)
            where getInd = elemIndex match
                  val = (\x y -> toInteger x + y) <$> getInd bstep <*> ((\x -> n * toInteger x) <$> getInd gstep)
  where 
      -- Try the first 50 powers of g before moving onto BSGS
      b = discreteLogBrute g h g 2 50

-- |Pollard's Rho algorithm for use in solving the discrete logarithm problem.
--pollardRhoDLog :: (Integral b, CyclicGroup a, FiniteGroup a, AbelianGroup a, Group a, Eq a) => a -> a -> Integer -> (a -> b) -> Maybe Integer
    {-
pollardRhoDLog g h ord classify = 
    let mapG x n
          | classify x == 1 = n
          | classify x == 2 = (n * 2) `mod` (ord + 1)
          | classify x == 3 = (n + 1) `mod` (ord + 1)
          | otherwise = error "Invalid classification function!"
        mapH x n
          | classify x == 1 = (n + 1) `mod` (ord + 1)
          | classify x == 2 = (n * 2) `mod` (ord + 1)
          | classify x == 3 = n
          | otherwise = error "Invalid classification function!"
        mix x
          | classify x == 1 = g `gcompose` x
          | classify x == 2 = x `gcompose` x
          | classify x == 3 = h `gcompose` x
          | otherwise = error "Invalid classification function!"
        x0 = gid g
        inner x a b x' a' b' =
            let xi = mix x
                ai = mapG x a
                bi = mapH x b
                x2i = mix (mix x')
                a2i = mapG x' (mapG x' a')
                b2i = mapH x' (mapH x' b')
             in if xi == x2i
                   --then (xi, ai, bi, x2i, a2i, b2i) : out
                   then rmul <$> Just (qrcoerce (ZZ (ai - a2i)) (ZZ ord) :: ZZN) <*> rinv (qrcoerce (ZZ (b2i - bi)) (ZZ ord) :: ZZN)
                   --else inner xi ai bi x2i a2i b2i ((xi, ai, bi, x2i, a2i, b2i) : out)
                   else inner xi ai bi x2i a2i b2i --((xi, ai, bi, x2i, a2i, b2i) : out)
        res = inner x0 0 0 x0 0 0 --[(x0, 0, 0, x0, 0, 0)]
     in res
     --in if isNothing res
     --      then Nothing
     --      else Just (toInteger (qrelement (fromJust res)))
           -}



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
logReduce :: (CyclicGroup a, FiniteGroup a, AbelianGroup a, Group a, Eq a, Integral b, Integral c) => a -> a -> b -> c -> Maybe Integer
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
pohligHellman :: (CyclicGroup a, FiniteGroup a, AbelianGroup a, Group a, Eq a) => a -> a -> [(Integer, Integer)] -> Maybe Integer
pohligHellman g h factors = if isNothing res
                               then Nothing
                               else return (fromIntegral . toInteger . qrelement $ fromJust res)
    where
        ord                    = foldr (\x acc -> acc * uncurry (^) x) 1 factors
        tmp t (q,e)            = gpow t (ord `R.div` (q^e))
        subproblem a b x@(q,e) = fromJust $ logReduce (tmp a x) (tmp b x) q e
        res                    = crtList (map (toInteger . subproblem g h) factors)
                                         (map (uncurry (^)) factors)

discreteLog :: (CyclicGroup a, FiniteGroup a, AbelianGroup a, Group a, Eq a, Integral b) => a -> a -> b -> Maybe Integer
discreteLog g h order
  | g == h = Just 1
  | b /= -1 = Just b
  | otherwise = pohligHellman g h factors
    where factors = peForm (factor pollardRhoF (toInteger order))
          b = discreteLogBrute g h g 2 50

