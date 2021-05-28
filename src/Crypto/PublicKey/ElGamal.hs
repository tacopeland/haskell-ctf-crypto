module Crypto.PublicKey.ElGamal where


import Data.List
import Data.Maybe
import Math.NumberTheory.Roots

import System.IO.Unsafe
import System.Random

import Crypto.Integers
import Crypto.Ring
import Crypto.QuotientRing


class ElGamalKey a where
    encrypt :: a -> Integer -> (Integer, Integer)


data ElGamalPubKey
    -- | Accepts the generator g, the modulus p, and the public key A.
    = ElGamalPubKey Integer Integer Integer
    deriving (Show)

data ElGamalPrivKey
    -- | Accepts the generator g, the modulus p, and the secret a.
    = ElGamalPrivKey Integer Integer Integer
    deriving (Show)

instance ElGamalKey ElGamalPubKey where
    encrypt (ElGamalPubKey g p pubA) m = (c1, c2)
        where k = unsafePerformIO $ randomRIO (2, p)
              Just (ZnZ (Z c1) _) = rpow (ZnZ (Z g) (Z p)) k
              (ZnZ (Z c2) _) = rmul (ZnZ (Z m) (Z p)) (fromJust $ rpow (ZnZ (Z pubA) (Z p)) k)

instance ElGamalKey ElGamalPrivKey where
    encrypt (ElGamalPrivKey g p a) m = (c1, c2)
        where Just (ZnZ (Z pubA) _) = rpow (ZnZ (Z g) (Z p)) a
              k = unsafePerformIO $ randomRIO (2, p)
              Just (ZnZ (Z c1) _) = rpow (ZnZ (Z g) (Z p)) k
              (ZnZ (Z c2) _) = rmul (ZnZ (Z m) (Z p)) (fromJust $ rpow (ZnZ (Z pubA) (Z p)) k)

decrypt :: ElGamalPrivKey -> (Integer, Integer) -> Integer
decrypt (ElGamalPrivKey g p a) (c1, c2) = m
    where Just x = rpow (ZnZ (Z c1) (Z p)) (-a)
          (ZnZ (Z m) _) = rmul x (ZnZ (Z c2) (Z p))

{-
constructNE :: Integer -> Integer -> ElGamalPubKey
constructNE = ElGamalPubKey

constructFacE :: [Integer] -> Integer -> ElGamalPrivKey
constructFacE factors e = ElGamalPrivKey factors n e d phi
    where n = product factors
          phi = product (map (flip (-) 1) factors)
          Just (ZnZ (Z d) _) = rinv (ZnZ (Z e) (Z phi))

-- | Only works on moduli of the form n = pq, where p and q are prime.
constructPhiNE :: Integer -> Integer -> Integer -> ElGamalPrivKey
constructPhiNE phi n e = ElGamalPrivKey factors n e d phi
    where b = phi - n - 1
          p = (-b - integerSquareRoot (b^2 - 4*n)) `div` 2
          q = (-b + integerSquareRoot (b^2 - 4*n)) `div` 2
          Just (ZnZ (Z d) _) = rinv (ZnZ (Z e) (Z phi))
          factors = [p, q]

constructNED :: Integer -> Integer -> Integer -> ElGamalPrivKey
constructNED n e d = ElGamalPrivKey factors n e d phi
    where kphi = e * d - 1
          reduceOdd t = if even t then reduceOdd (t `div` 2) else t
          t = reduceOdd kphi
          ks = takeWhile (< kphi) (iterate (*2) t)
          as = [2,4..100]
          candidates = (\a k -> fromJust (rpow (ZnZ (Z a) (Z n)) k)) <$> as <*> ks
          Just (ZnZ (Z z) _) = find (\x -> x /= rid x && x /= rneg (rid x) && fromJust (rpow x 2) == rid x) candidates
          (p, _, _) = intXgcd (z + 1) n
          q = n `div` p
          factors = [p, q]
          phi = (p - 1) * (q - 1)

privToPub :: ElGamalPrivKey -> ElGamalPubKey
privToPub (ElGamalPrivKey _ n e _ _) = ElGamalPubKey n e
-}
