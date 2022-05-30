module Crypto.PublicKey.ElGamal where

import NumberTheory.Modular
import Algebra.Structure.Ring
import Algebra.Structure.Field
import Algebra.ZZ
import Algebra.ZZP

import Data.List
import Data.Maybe
import Math.NumberTheory.Roots

import System.Random


data ElGamalKey =
    -- | Accepts the generator g, the modulus p, and the public key A.
      ElGamalPubKey {keyG :: Integer, keyP :: Integer, keyPubA :: Integer}
    -- | Accepts the generator g, the modulus p, and the secret a.
    | ElGamalPrivKey {keyG :: Integer, keyP :: Integer, keyPrivA :: Integer}
    deriving (Show)

encrypt (ElGamalPubKey g p pubA) m = (c1, c2)
    where stdGen  = mkStdGen 0xdeadcafe
          (k, _)  = randomR (2, p) stdGen
          Just c1 = modPow g k p
          c2      = (m * fromJust (modPow pubA k p)) `mod` p

encrypt (ElGamalPrivKey g p a) m = (c1, c2)
    where stdGen    = mkStdGen 0xdeadcafe
          (k, _)    = randomR (2, p) stdGen
          Just pubA = modPow g a p
          Just c1   = modPow g k p
          c2        = (m * fromJust (modPow pubA k p)) `mod` p

decrypt :: ElGamalKey -> (Integer, Integer) -> Maybe Integer
decrypt (ElGamalPubKey {}) _ = Nothing
decrypt (ElGamalPrivKey g p a) (c1, c2) = m
    where x = modPow c1 (-1) p
          m = x >>= (\x' -> Just ((x' * c2) `mod` p))

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
