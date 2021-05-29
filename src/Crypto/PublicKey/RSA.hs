module Crypto.PublicKey.RSA where


import Data.List
import Data.Maybe
import Math.NumberTheory.Roots

import Crypto.Integers
import Crypto.Algebra.Ring.Class
import Crypto.Algebra.Ring.QuotientRing
import Crypto.Algebra.ZZ
import Crypto.Algebra.ZZP


class RSAKey a where
    encrypt :: a -> Integer -> Integer


data RSAPubKey
    -- | Accepts the modulus n and private exponent e as arguments
    = RSAPubKey Integer Integer
    deriving (Show)

data RSAPrivKey
    -- | Arguments are [factors], n, e, d, phi
    = RSAPrivKey [Integer] Integer Integer Integer Integer
    deriving (Show)

instance RSAKey RSAPubKey where
    encrypt (RSAPubKey n e) m = c
        where Just (ZZP (ZZ c) _) = rpow (ZZP (ZZ m) (ZZ n)) e

instance RSAKey RSAPrivKey where
    encrypt (RSAPrivKey _ n e _ _) m = c
        where Just (ZZP (ZZ c) _) = rpow (ZZP (ZZ m) (ZZ n)) e

decrypt :: RSAPrivKey -> Integer -> Integer
decrypt (RSAPrivKey _ n _ d _) c = m
    where Just (ZZP (ZZ m) _) = rpow (ZZP (ZZ c) (ZZ n)) d

constructNE :: Integer -> Integer -> RSAPubKey
constructNE = RSAPubKey

constructFacE :: [Integer] -> Integer -> RSAPrivKey
constructFacE factors e = RSAPrivKey factors n e d phi
    where n = product factors
          phi = product (map (flip (-) 1) factors)
          Just (ZZP (ZZ d) _) = rinv (ZZP (ZZ e) (ZZ phi))

-- | Only works on moduli of the form n = pq, where p and q are prime.
constructPhiNE :: Integer -> Integer -> Integer -> RSAPrivKey
constructPhiNE phi n e = RSAPrivKey factors n e d phi
    where b = phi - n - 1
          p = (-b - integerSquareRoot (b^2 - 4*n)) `div` 2
          q = (-b + integerSquareRoot (b^2 - 4*n)) `div` 2
          Just (ZZP (ZZ d) _) = rinv (ZZP (ZZ e) (ZZ phi))
          factors = [p, q]

constructNED :: Integer -> Integer -> Integer -> RSAPrivKey
constructNED n e d = RSAPrivKey factors n e d phi
    where kphi = e * d - 1
          reduceOdd t = if even t then reduceOdd (t `div` 2) else t
          t = reduceOdd kphi
          ks = takeWhile (< kphi) (iterate (*2) t)
          as = [2,4..100]
          candidates = (\a k -> fromJust (rpow (ZZP (ZZ a) (ZZ n)) k)) <$> as <*> ks
          Just (ZZP (ZZ z) _) = find (\x -> x /= rid x && x /= rneg (rid x) && fromJust (rpow x 2) == rid x) candidates
          (p, _, _) = xgcd (z + 1) n
          q = n `div` p
          factors = [p, q]
          phi = (p - 1) * (q - 1)

privToPub :: RSAPrivKey -> RSAPubKey
privToPub (RSAPrivKey _ n e _ _) = RSAPubKey n e
