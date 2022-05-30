module Crypto.PublicKey.RSA where

import NumberTheory.Basic
import NumberTheory.Modular
import Algebra.Structure.Ring
import Algebra.ZZ
import Algebra.ZZP

import Data.List
import Data.Maybe
import Math.NumberTheory.Roots



data RSAKey =
    -- | Accepts the modulus n and private exponent e as arguments
      RSAPubKey { keyN :: Integer, keyE :: Integer}
    -- | Arguments are [factors], n, e, d, phi
    | RSAPrivKey { keyFactors :: [Integer]
                 , keyN :: Integer
                 , keyE :: Integer
                 , keyD :: Integer
                 , keyPhi :: Integer
                 }
    deriving (Show)


encrypt :: Integer -> RSAKey -> Integer
encrypt msg key = fromJust $ modPow msg (keyE key) (keyN key)

decrypt :: Integer -> RSAKey -> Maybe Integer
decrypt ct key@(RSAPrivKey {}) = modPow ct (keyD key) (keyN key)
decrypt ct (RSAPubKey {}) = Nothing

constructNE :: Integer -> Integer -> RSAKey
constructNE = RSAPubKey

constructFacE :: [Integer] -> Integer -> RSAKey
constructFacE factors e = RSAPrivKey factors n e d phi
    where n      = product factors
          phi    = product (map (flip (-) 1) factors)
          Just d = modInv e phi

-- | Only works on moduli of the form n = pq, where p and q are prime.
constructPhiNE :: Integer -> Integer -> Integer -> RSAKey
constructPhiNE phi n e = RSAPrivKey factors n e d phi
    where b = phi - n - 1
          p = (-b - integerSquareRoot (b^2 - 4*n)) `div` 2
          q = (-b + integerSquareRoot (b^2 - 4*n)) `div` 2
          Just d = modInv e phi
          factors = [p, q]

constructNED :: Integer -> Integer -> Integer -> RSAKey
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
