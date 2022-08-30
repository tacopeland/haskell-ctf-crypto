module CTF.Crypto.PublicKey.RSA where

import CTF.Crypto.Util.Encoding
import NumberTheory.Basic
import NumberTheory.Modular
import Algebra.Structure.Ring
import Algebra.ZZ
import Algebra.ZZP

import Crypto.PubKey.RSA.Types (PublicKey(..))
import Data.ASN1.BinaryEncoding (DER(..))
import Data.ASN1.Encoding (decodeASN1', encodeASN1')
import Data.ASN1.Error (ASN1Error(..))
import Data.ASN1.Types (ASN1(..), ASN1ConstructionType(..))
import qualified Data.ByteString as B
import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.PEM (PEM(..), pemParseBS, pemWriteBS)
import qualified Data.X509 as X509
import Math.NumberTheory.Roots (integerSquareRoot)



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
          Just (ZZP (ZZ z) _) = L.find (\x -> x /= rid x && x /= rneg (rid x) && fromJust (rpow x 2) == rid x) candidates
          (p, _, _) = xgcd (z + 1) n
          q = n `div` p
          factors = [p, q]
          phi = (p - 1) * (q - 1)


readDER :: B.ByteString -> Either ASN1Error RSAKey
readDER bytes =
    case decodeASN1' DER bytes of
        Left err' -> Left err'
        Right [ Start Sequence
              , IntVal n -- modulus
              , IntVal e -- public exponent
              , End Sequence 
              ] -> Right (RSAPubKey n e)
        Right [ Start Sequence
              , IntVal _ -- version
              , IntVal n -- modulus
              , IntVal e -- public exponent
              , IntVal d -- private exponent
              , IntVal p
              , IntVal q
              , IntVal _ -- d mod (p-1)
              , IntVal _ -- d mod (q-1)
              , IntVal _ -- modInv q p
              , End Sequence 
              ] -> Right (RSAPrivKey [p,q] n e d ((p-1)*(q-1)))
        x -> Left (TypeNotImplemented "Invalid key format")

toDER :: RSAKey -> B.ByteString
toDER (RSAPubKey n e) = encodeASN1' DER [Start Sequence, IntVal n, IntVal e, End Sequence]
toDer (RSAPrivKey [p,q] n e d _) =
    encodeASN1' DER
      [ Start Sequence, IntVal 0
      , IntVal n, IntVal e, IntVal d, IntVal p, IntVal q
      , IntVal (d `mod` (p-1)), IntVal (d `mod` (q-1))
      , IntVal (fromJust $ modInv q p)
      , End Sequence ]

-- RSA key syntax is here: https://www.rfc-editor.org/rfc/rfc8017#appendix-A.1
constructPEM :: FilePath -> IO RSAKey
constructPEM file = do
    rawPEM <- B.readFile file
    case pemParseBS rawPEM of
      Left err -> error err
      Right (pemParsed:_) ->
          case readDER (pemContent pemParsed) of
            Left err' -> error (show err')
            Right key -> return key
      _ -> error "Error: didn't parse a key"

savePEM :: FilePath -> RSAKey -> IO ()
savePEM file key@(RSAPubKey {}) = B.writeFile file (pemWriteBS (PEM "RSA PUBLIC KEY" [] (toDER key)))
savePEM file key@(RSAPrivKey {}) = B.writeFile file (pemWriteBS (PEM "RSA PRIVATE KEY" [] (toDER key)))

constructDER :: FilePath -> IO RSAKey
constructDER file = do
    rawDER <- B.readFile file
    case X509.decodeSignedCertificate rawDER of
      Left err -> error err
      Right cert ->
          case X509.certPubKey (X509.signedObject (X509.getSigned cert)) of
            X509.PubKeyRSA key -> return (RSAPubKey (public_n key) (public_e key))
            x -> error ("Invalid public key: " ++ show x)

readSSHPub :: B.ByteString -> RSAKey
readSSHPub bs =
    let typeLen = fromInteger $ bytesToInteger (B.take 4 bs)
        rest = B.drop (4 + typeLen) bs
        eLen = fromInteger $ bytesToInteger (B.take 4 rest)
        e = bytesToInteger (B.take eLen (B.drop 4 rest))
        rest' = B.drop (4 + eLen) rest
        nLen = fromInteger $ bytesToInteger (B.take 4 rest')
        n = bytesToInteger (B.take nLen (B.drop 4 rest'))
     in RSAPubKey n e
