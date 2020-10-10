module Crypto where

import Crypto.Group
import Crypto.Ring
import Crypto.Field

import Data.List
import Data.Maybe

import Debug.Trace


mulmod :: (Integral a) => [a] -> a -> Maybe ZmodN
mulmod [] _ = Nothing
mulmod (x:[]) m = Just (ofIntegral x m)
mulmod (x:xs) m = Just (ofIntegral (toInteger x * p) m)
    where Just (ZmodN p _) = mulmod xs m

-- |Finds the order of an element in its group (naive)
order :: ZmodN -> Maybe ZmodN
order (ZmodN 0 _) = Nothing
order n
    | gcd (bareInteger n) (modulus n) /= 1 = Nothing
    | otherwise = Just (ofIntegral (toInteger index + 1) (modulus n))
    where Just index = elemIndex identity (iterate (gcompose n) n)
          identity = ofIntegral 1 (modulus n)

babystep :: ZmodN -> [ZmodN]
babystep n = iterate (gcompose n) (ofIntegral 1 (modulus n))

giantstep :: ZmodN -> ZmodN -> [ZmodN]
giantstep h u = iterate (gcompose u) h

-- |Shanks' Baby-Step Giant-Step discrete logarithm algorithm
bsgs :: ZmodN -> ZmodN -> ZmodN -> Maybe ZmodN
bsgs g h ord =
    let 
        n = (floor . sqrt. fromIntegral) (bareInteger ord) + 1
        u = gpow g (-n)
        bstep = take n $ babystep g
        gstep
            | u == Nothing = []
            | otherwise    = take n $ giantstep h (fromJust u)
    in case intersect bstep gstep of
        [] -> Nothing
        (match:_) -> flip ofIntegral (bareInteger ord) <$> (toInteger <$> bare_int) 
            where getInd = elemIndex match
                  val = (+) <$> (getInd bstep) <*> ((n*) <$> (getInd gstep))
                  bare_int = flip mod (fromIntegral $ bareInteger ord) <$> val

crt :: [ZmodN] -> Maybe ZmodN
crt []       = Nothing
crt (a:[])   = Just a
crt (a:b:cs) =
    let 
        y = rmul <$> Just (b `rsub` (ofIntegral (bareInteger a) (modulus b))) <*> modinv (ofIntegral (modulus a) (modulus b))
    in case y of
        Nothing          -> Nothing
        Just (ZmodN x _) -> crt (ofIntegral (bareInteger a + modulus a * x) (modulus a * modulus b) : cs)

-- |Algorithm to reduce discrete logarithm for an element with prime power order
logreduce :: (Integral a) => ZmodN -> ZmodN -> ZmodN -> a -> Maybe ZmodN
logreduce g h base exp
    | exp <= 0  = Nothing
    | otherwise =
    let
        -- (g^base^(exp-1)^x_n = (h*g^(-x))^(exp-1-n)
        -- (m^x_n = (h * g^(-x)) ^ (exp-1-n)
        base_i      = bareInteger base
        pow_pow a b = pow a (base_i ^ b)
        negpow x    = pow g (-x)
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
        tmp t (q,e)            = fromJust $ pow t (ord `div` (q^e))
        subproblem a b x@(q,e) = fromJust $ logreduce (tmp a x) (tmp b x) (ZmodN q (modulus a)) e
