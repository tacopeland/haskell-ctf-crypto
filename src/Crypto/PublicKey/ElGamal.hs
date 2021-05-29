module Crypto.PublicKey.ElGamal where


import Data.List
import Data.Maybe
import Math.NumberTheory.Roots

import System.IO.Unsafe
import System.Random

import Crypto.Integers
import Crypto.Algebra.Ring.Class
import Crypto.Algebra.Field.Class
import Crypto.Algebra.ZZ
import Crypto.Algebra.ZZP


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
              (ZZP (ZZ c1) _) = fpow (ZZP (ZZ g) (ZZ p)) k
              (ZZP (ZZ c2) _) = rmul (ZZP (ZZ m) (ZZ p)) (fromJust $ rpow (ZZP (ZZ pubA) (ZZ p)) k)

instance ElGamalKey ElGamalPrivKey where
    encrypt (ElGamalPrivKey g p a) m = (c1, c2)
        where Just (ZZP (ZZ pubA) _) = rpow (ZZP (ZZ g) (ZZ p)) a
              k = unsafePerformIO $ randomRIO (2, p)
              (ZZP (ZZ c1) _) = fpow (ZZP (ZZ g) (ZZ p)) k
              (ZZP (ZZ c2) _) = rmul (ZZP (ZZ m) (ZZ p)) (fromJust $ rpow (ZZP (ZZ pubA) (ZZ p)) k)

decrypt :: ElGamalPrivKey -> (Integer, Integer) -> Integer
decrypt (ElGamalPrivKey g p a) (c1, c2) = m
    where Just x = rpow (ZZP (ZZ c1) (ZZ p)) (-a)
          (ZZP (ZZ m) _) = rmul x (ZZP (ZZ c2) (ZZ p))

{-
constructNE :: Integer -> Integer -> ElGamalPubKey
constructNE = ElGamalPubKey

constructFacE :: [Integer] -> Integer -> ElGamalPrivKey
constructFacE factors e = ElGamalPrivKey factors n e d phi
    where n = product factors
          phi = product (map (flip (-) 1) factors)
          Just (ZZP (ZZ d) _) = rinv (ZZP (ZZ e) (ZZ phi))

-- | Only works on moduli of the form n = pq, where p and q are prime.
constructPhiNE :: Integer -> Integer -> Integer -> ElGamalPrivKey
constructPhiNE phi n e = ElGamalPrivKey factors n e d phi
    where b = phi - n - 1
          p = (-b - integerSquareRoot (b^2 - 4*n)) `div` 2
          q = (-b + integerSquareRoot (b^2 - 4*n)) `div` 2
          Just (ZZP (ZZ d) _) = rinv (ZZP (ZZ e) (ZZ phi))
          factors = [p, q]

constructNED :: Integer -> Integer -> Integer -> ElGamalPrivKey
constructNED n e d = ElGamalPrivKey factors n e d phi
    where kphi = e * d - 1
          reduceOdd t = if even t then reduceOdd (t `div` 2) else t
          t = reduceOdd kphi
          ks = takeWhile (< kphi) (iterate (*2) t)
          as = [2,4..100]
          candidates = (\a k -> fromJust (rpow (ZZP (ZZ a) (ZZ n)) k)) <$> as <*> ks
          Just (ZZP (ZZ z) _) = find (\x -> x /= rid x && x /= rneg (rid x) && fromJust (rpow x 2) == rid x) candidates
          (p, _, _) = intXgcd (z + 1) n
          q = n `div` p
          factors = [p, q]
          phi = (p - 1) * (q - 1)

privToPub :: ElGamalPrivKey -> ElGamalPubKey
privToPub (ElGamalPrivKey _ n e _ _) = ElGamalPubKey n e
-}
