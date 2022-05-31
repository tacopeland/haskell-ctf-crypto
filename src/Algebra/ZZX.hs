module Algebra.ZZX (ZZX(..), degree) where

import Helpers

import NumberTheory.Integers
import NumberTheory.Modular
import NumberTheory.Primes
import Algebra.Structure.Ring
import Algebra.ZZ

import Data.Maybe



data ZZX =
      ZZXCoefficients { coefficients :: [ZZ] }
    | ZZXPointValue { ys :: [ZZ], omega :: ZZ, modulus :: ZZ }
    deriving (Show, Eq)


    {-
       Internal typeclasses
    -}

instance Ring ZZX where
    rzero (ZZXCoefficients coeffs) =
        ZZXCoefficients (replicate (length coeffs) 0)
    rzero (ZZXPointValue ys omega modulus) =
        ZZXPointValue (replicate (length ys) 0) omega modulus

    radd (ZZXCoefficients c1) (ZZXCoefficients c2) =
        ZZXCoefficients (zipWith (+) c1 c2)
    radd c1 c2 = error "Both arguments to radd must be in coefficient representation."

    rmul (ZZXPointValue u omega modulus) (ZZXPointValue v omega' modulus')
        | modulus == modulus' && omega == omega' =
            ZZXPointValue (zipWith (\a b -> modMul a b modulus) u v) omega modulus
        | modulus == modulus' = error "Conflicting omega in point-value representations."
        | omega == omega      = error "Conflicting modulus in point-value representations."
        | otherwise           = error "Conflicting omega and modulus in point-value representations."
    rmul c1@(ZZXCoefficients {}) c2@(ZZXCoefficients {}) =
        let n = toInteger $ degree c1 + degree c2 + 1
            maxCoeff = foldr (\(ZZ x) -> max x) 0
                             (mappend (coefficients c1) (coefficients c2))
            minModulus = maxCoeff * maxCoeff * n + 1
            findModulus k =
                let newN = k*n + 1
                 in if newN >= minModulus && isPrime newN
                       then newN
                       else findModulus (k+1)
            modulus = ZZ $ findModulus (minModulus `div` n)
            omega = ZZ $ fromJust $ primitiveRootOfUnity n (toInteger modulus)
            x = nttPoly (padPoly c1 (fromIntegral n)) omega modulus
            y = nttPoly (padPoly c2 (fromIntegral n)) omega modulus
            z = ZZXPointValue (zipWith (\a b -> modMul a b modulus) (ys x) (ys y))
                              omega modulus
            shift p = last p : init p
         in ZZXCoefficients (shift (coefficients $ invNttPoly z))
    rmul _ _ = error "Both arguments to rmul must be in the same representation."

    rneg (ZZXCoefficients coeffs) = ZZXCoefficients (map (\x -> -x) coeffs)
    rneg _ = undefined

    rpow p@(ZZXCoefficients coeffs) e' =
        let e = fromIntegral e'
            n = toInteger $ degree p * e + 1
            maxCoeff = foldr (\(ZZ x) -> max x) 0 coeffs
            minModulus = maxCoeff^e * n + 1
            findModulus k =
                let newN = k*n + 1
                 in if newN >= minModulus && isPrime newN
                       then newN
                       else findModulus (k+1)
            modulus = ZZ $ findModulus (minModulus `div` n)
            omega = ZZ $ fromJust $ primitiveRootOfUnity n (toInteger modulus)
            pvPoly = nttPoly (padPoly p (fromIntegral n)) omega modulus
            identityPv = ZZXPointValue (replicate (fromIntegral n) (ZZ 1)) omega modulus
            z = squareAndMultiply (\x y -> x {ys = zipWith (\a b -> modMul a b modulus) (ys x) (ys y)})
                                    identityPv pvPoly e
            shift n p = if n == 0 then p else shift (n-1) (last p : init p)
         in Just $ ZZXCoefficients (shift (e-1) (coefficients $ invNttPoly z))

    rpow _ _ = Nothing

    rinv _ = Nothing


    {-
       For local use
    -}

squareAndMultiplyCount :: (Integral b) => (a -> a -> a) -> a -> a -> b -> (a, Int)
squareAndMultiplyCount operation identity base exponent =
    let s = zipWith (\a e -> if e == 1 then a else identity)
                (squares operation base)
                (binexpand exponent)
     in (foldr operation identity s, length s)


nttPoly :: ZZX -> ZZ -> ZZ -> ZZX
nttPoly poly@(ZZXPointValue {}) _ _ = poly
nttPoly (ZZXCoefficients coeffs) omega modulus =
    let ys = map ZZ (ntt (map toInteger coeffs)
                         (toInteger omega)
                         (toInteger modulus))
     in ZZXPointValue ys omega modulus

invNttPoly :: ZZX -> ZZX
invNttPoly poly@(ZZXCoefficients {}) = poly
invNttPoly (ZZXPointValue ys omega modulus) =
    let coeffs = map ZZ (inverseNtt (map toInteger ys)
                                    (toInteger omega)
                                    (toInteger modulus))
     in ZZXCoefficients coeffs

padPoly :: ZZX -> Int -> ZZX
padPoly (ZZXCoefficients coeffs) size =
    ZZXCoefficients (mappend (replicate (size - length coeffs) (ZZ 0)) coeffs)
padPoly (ZZXPointValue ys o n) size =
    ZZXPointValue (mappend (replicate (size - length ys) (ZZ 0)) ys) o n


    {-
       Exported
    -}

degree :: ZZX -> Int
degree (ZZXCoefficients coeffs) =
    let actualCoeffs = dropWhile (\x -> x == rzero x) coeffs
     in length actualCoeffs - 1
degree _ = error "Unsupported for point-value representation."
