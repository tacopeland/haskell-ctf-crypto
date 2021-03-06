{-# LANGUAGE TemplateHaskell #-}
module CryptoTests where

import Test.QuickCheck
import Test.QuickCheck.All

import Data.Maybe

import GHC.Real as R

import Crypto.Integers
import Crypto.Algebra.Group.Class
import Crypto.Algebra.Ring.Class
import Crypto.Algebra.Ring.QuotientRing
import Crypto.Algebra.Field.Class
import Crypto.Algebra.Domain.Class as D
import Crypto.Algebra.ZZ
import Crypto.Algebra.ZZP
import Crypto.Algebra.EC
import Crypto.Algebra.Generic
import Crypto.Algebra.Factor

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

    {-
-- BSGS
--prop_bsgsZZP :: BSGSZZP -> Bool
--prop_bsgsZZP (BSGSZZP (g@(ZZP _ p), h, order)) =
    let res = bsgs g h order in
        if isNothing res
           then if order /= p then True else False
           else gpow g (fromJust res) == h

-- Pohlig-Hellman (I'll make more tests when I have a factorization routine)
--prop_logreduceZZP :: BSGSZZP -> Bool
--prop_logreduceZZP (BSGSZZP (g, h, order)) =
    let res1 = logreduce g h order 1
        res2 = bsgs g h order
     in res1 == res2
     -}

    {-
prop_discreteLog :: Bool
prop_discreteLog =
    let
        p = ZZ 99061670249353652702595159229088680425828208953931838069069584252923270946291
        x = ZZ 6082896373499126624029343293750138460137531774473450341235217699497602895121
        a = ZZ 1
        b = ZZ 4
        ord = 99061670249353652702595159229088680426160873357666659718134032418967620849171
        g = liftX (ZZP x p) (ZZP a p) (ZZP b p)
     in discreteLog g (100 .* g) (peForm (factor pollardRhoF ord)) == Just 100

prop_factor :: Bool
prop_factor =
    let n = 580642391898843192929563856870897799650883152718761762932292482252152591279871421569162037190419036435041797739880389529593674485555792234900969402019055601781662044515999210032698275981631376651117318677368742867687180140048715627160641771118040372573575479330830092989800730105573700557717146251860588802509310534792310748898504394966263819959963273509119791037525504422606634640173277598774814099540555569257179715908642917355365791447508751401889724095964924513196281345665480688029639999472649549163147599540142367575413885729653166517595719991872223011969856259344396899748662101941230745601719730556631637
     in length (factor fermatFactor n) == 30

     -}

return []
runTests :: IO Bool
runTests = $forAllProperties $
    quickCheckWithResult (stdArgs {maxSuccess = 1000})
