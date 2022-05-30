{-# LANGUAGE TemplateHaskell #-}
module CryptoTests where

import Test.QuickCheck
import Test.QuickCheck.All

import Data.Maybe

import NumberTheory.Primes
import NumberTheory.Factor
import Algebra.Structure.Group
import Algebra.Structure.Ring
import Algebra.Structure.Field
import Algebra.Structure.Domain
import Algebra.ZZ
import Algebra.ZZN
import Algebra.ZZP
import Algebra.EC
import Algebra.Generic

-- https://stackoverflow.com/a/5055626
-- Add better one later, this is slow
newtype Prime = Prime Integer deriving Show

primes = filter isPrime [2..]

instance Arbitrary Prime where
    arbitrary = do i <- arbitrary
                   return $ Prime (primes!!abs i)

getcoprime :: Integer -> Gen Integer
getcoprime n = do a <- chooseInteger (1, n-1)
                  if gcd a n == 1 then return a else getcoprime n


-- Generate random valid ZZP for BSGS
newtype BSGSZZP = BSGSZZP (ZZP, ZZP, Integer) deriving Show

instance Arbitrary BSGSZZP where
    arbitrary = do (Prime p) <- arbitrary :: Gen Prime
                   a <- chooseInteger (1, p - 1)
                   b <- chooseInteger (1, p - 1)
                   return $ BSGSZZP (qrcoerce (ZZ a) (ZZ p), qrcoerce (ZZ b) (ZZ p), gorder (qrcoerce (ZZ a) (ZZ p) :: ZZP))
                   
-- ZZ
prop_ZZCommutativeAdd :: Integer -> Integer -> Bool
prop_ZZCommutativeAdd x y =
    ZZ x + ZZ y == ZZ y + ZZ x

prop_ZZAssociativeAdd :: Integer -> Integer -> Integer -> Bool
prop_ZZAssociativeAdd x y z =
    (ZZ x + ZZ y) + ZZ z == ZZ x + (ZZ y + ZZ z)

prop_ZZCommutativeMul :: Integer -> Integer -> Bool
prop_ZZCommutativeMul x y =
    ZZ x * ZZ y == ZZ y * ZZ x

prop_ZZAssociativeMul :: Integer -> Integer -> Integer -> Bool
prop_ZZAssociativeMul x y z =
    (ZZ x * ZZ y) * ZZ z == ZZ x * (ZZ y * ZZ z)

prop_ZZInverse :: Integer -> Bool
prop_ZZInverse x =
    let n = ZZ x in
        case rinv n of
          Nothing -> True
          Just inv -> inv * n == rid n && n * inv == rid n

prop_ZZInversePow :: Integer -> Bool
prop_ZZInversePow x =
    let n = ZZ x in
        rinv n == rpow n (-1)

prop_ZZIdentity :: Integer -> Bool
prop_ZZIdentity x =
    let n = ZZ x in
        rid n * n == n &&
        n * rid n == n


-- ZZP
prop_ZZPAddIdentity :: Integer -> Prime -> Bool
prop_ZZPAddIdentity x (Prime p) =
    let a = qrcoerce (ZZ x) (ZZ p) :: ZZP
     in a + rzero a == rzero a + a &&
         a + rzero a == a

prop_ZZPAddAssociative :: Integer -> Integer -> Integer -> Prime -> Bool
prop_ZZPAddAssociative x y z (Prime p) =
    let a = qrcoerce (ZZ x) (ZZ p) :: ZZP
        b = qrcoerce (ZZ y) (ZZ p) :: ZZP
        c = qrcoerce (ZZ z) (ZZ p) :: ZZP
     in ((a + b) + c) == (a + (b + c))

prop_ZZPAddCommutative :: Integer -> Integer -> Prime -> Bool
prop_ZZPAddCommutative x y (Prime p) =
    let a = qrcoerce (ZZ x) (ZZ p) :: ZZP
        b = qrcoerce (ZZ y) (ZZ p) :: ZZP
     in a + b == b + a

prop_ZZPMulAssociative :: Integer -> Integer -> Integer -> Prime -> Bool
prop_ZZPMulAssociative x y z (Prime p) =
    let a = qrcoerce (ZZ x) (ZZ p) :: ZZP
        b = qrcoerce (ZZ y) (ZZ p) :: ZZP
        c = qrcoerce (ZZ z) (ZZ p) :: ZZP
     in ((a * b) * c) == (a * (b * c))

prop_ZZPMulCommutative :: Integer -> Integer -> Prime -> Bool
prop_ZZPMulCommutative x y (Prime p) =
    let a = qrcoerce (ZZ x) (ZZ p) :: ZZP
        b = qrcoerce (ZZ y) (ZZ p) :: ZZP
     in a * b == b * a

prop_ZZPDistributive :: Integer -> Integer -> Integer -> Prime -> Bool
prop_ZZPDistributive x y z (Prime p) =
    let a = qrcoerce (ZZ x) (ZZ p) :: ZZP
        b = qrcoerce (ZZ y) (ZZ p) :: ZZP
        c = qrcoerce (ZZ z) (ZZ p) :: ZZP
     in a * (b + c) == (a * b) + (a * c)

prop_ZZPAddInverse :: Integer -> Prime -> Bool
prop_ZZPAddInverse x (Prime p) =
    let a = qrcoerce (ZZ x) (ZZ p) :: ZZP
     in a - a == (-a) + a &&
         a - a == rzero a

prop_ZZPMulInverse :: Integer -> Prime -> Bool
prop_ZZPMulInverse x (Prime p) =
    let n = qrcoerce (ZZ x) (ZZ p) :: ZZP
     in n == rzero n ||
        finv n * n == rid n &&
        n * finv n == rid n

prop_ZZPInversePow :: Integer -> Prime -> Bool
prop_ZZPInversePow x (Prime p) =
    let n = qrcoerce (ZZ x) (ZZ p) :: ZZP
     in finv n == fpow n (-1)

prop_ZZPFermatLittle :: Integer -> Prime -> Bool
prop_ZZPFermatLittle x (Prime p) =
    let n = qrcoerce (ZZ x) (ZZ p) :: ZZP
     in fpow n p == n

prop_ZZPMulIdentity :: Integer -> Prime -> Bool
prop_ZZPMulIdentity x (Prime p) =
    let n = qrcoerce (ZZ x) (ZZ p) :: ZZP
     in rid n * n == n &&
        n * rid n == n


return []
runTests :: IO Bool
runTests = $forAllProperties $
    quickCheckWithResult (stdArgs {maxSuccess = 1000})
