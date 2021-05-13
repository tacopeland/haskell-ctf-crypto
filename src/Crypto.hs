{-# LANGUAGE ScopedTypeVariables #-}
module Crypto where

import Crypto.Group
import Crypto.Ring
import Crypto.QuotientRing
import Crypto.Field

import Data.List
import Data.Maybe

import Debug.Trace


-- |Shanks' Baby-Step Giant-Step discrete logarithm algorithm
bsgs :: (Integral b, FiniteGroup a, AbelianGroup a, Group a, Eq a) => a -> a -> b -> Maybe Int
bsgs g h ord =
    let 
        babystep n = iterate (gcompose n) (gid n)
        giantstep h u = iterate (gcompose u) h
        n = (floor . sqrt . fromIntegral) ord + 1
        u = gpow g (-n)
        bstep = take n $ babystep g
        gstep = take n $ giantstep h u
    -- Get the output from this match
    in case intersect bstep gstep of
        [] -> Nothing
        (match:_) -> val
            where getInd = elemIndex match
                  val = (+) <$> (getInd bstep) <*> ((n*) <$> (getInd gstep))

-- Need to make some quotient rings for this
crt :: forall a b . (Ring a, Ring b, (QuotientRing a b)) => [a] -> Maybe a
crt []       = Nothing
crt (aa:[])   = Just aa
crt (aa:bb:cs) =
    let 
        p = (qrelement aa) 
        q = (qrideal aa)
        r = (qrelement bb)
        s = (qrideal bb)
        t = (qrcoerce r s) :: a
        u = (qrcoerce p s) :: a
        v = (qrcoerce q s) :: a
        y = rmul <$> (Just (t `radd` (rneg u))) <*> (rinv v)
    in case y of
        Nothing          -> Nothing
        Just ans -> crt (qrcoerce (p `radd` (q `rmul` (qrelement ans))) (q `rmul` s) : cs)

    {-
-- |Algorithm to reduce discrete logarithm for an element with prime power order
logreduce :: (Integral a) => ZmodN -> ZmodN -> ZmodN -> a -> Maybe ZmodN
logreduce g h base exp
    | exp <= 0  = Nothing
    | otherwise =
    let
        -- (g^base^(exp-1)^x_n = (h*g^(-x))^(exp-1-n)
        -- (m^x_n = (h * g^(-x)) ^ (exp-1-n)
        base_i      = zmodn_asInteger base
        pow_pow a b = gpow a (base_i ^ b)
        negpow x    = gpow g (-x)
        Just m      = pow_pow g (exp-1)

        x_n 0 = do
                b             <- pow_pow h (exp-1)
                bsgs m b base
        x_n n = do
                ZmodN prevx _ <- x_n (n-1)
                rterm         <- negpow (prevx)
                b             <- pow_pow (h `gcompose` rterm) (exp-1-n)
                ZmodN xn _    <- bsgs m b base
                Just $ (ZmodN xn (base_i ^ (n+1))) `rmul` (ZmodN (base_i ^ n) (base_i ^ (n+1))) `radd` (ZmodN prevx (base_i ^ (n+1)))
                
    in
        x_n (exp - 1)


pohlig_hellman :: ZmodN -> ZmodN -> [(Integer, Integer)] -> Maybe ZmodN
pohlig_hellman g h factors = crt $ foldr (\x acc -> subproblem g h x : acc) [] factors
    where
        ord                    = foldr (\x acc -> acc * ((fst x)^(snd x))) 1 factors
        tmp t (q,e)            = fromJust $ gpow t (ord `div` (q^e))
        subproblem a b x@(q,e) = fromJust $ logreduce (tmp a x) (tmp b x) (ZmodN q (modulus a)) e
-}
