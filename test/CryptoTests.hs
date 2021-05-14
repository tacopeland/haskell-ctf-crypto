{-# LANGUAGE TemplateHaskell #-}
module CryptoTests where

import Test.QuickCheck
import Test.QuickCheck.All

import Data.Maybe

import Crypto.Group
import Crypto.Ring
import Crypto

-- https://stackoverflow.com/a/5055626
-- Add better one later, this is slow
newtype Prime = Prime Integer deriving Show

primes = sieve [2..]
    where
      sieve (p:xs) = Prime p : sieve [x | x <- xs, x `mod` p > 0]

instance Arbitrary Prime where
    arbitrary = do i <- arbitrary
                   return $ primes!!(abs i)

getcoprime :: Integer -> Gen Integer
getcoprime n = do a <- chooseInteger (1, (n-1))
                  if gcd a n == 1 then return a else getcoprime n

-- Generate random valid ZmodN for BSGS
newtype BSGSZmodN = BSGSZmodN (ZmodN, ZmodN, Integer) deriving Show

instance Arbitrary BSGSZmodN where
    arbitrary = do (Positive n) <- arbitrary :: Gen (Positive Integer)
                   n <- return $ n + 1
                   a <- getcoprime n
                   b <- choose (1, (n - 1))
                   return $ BSGSZmodN (ZmodN a n, ZmodN b n, gorder (ZmodN a n))
                   

-- Generate random valid ZmodP for BSGS
newtype BSGSZmodP = BSGSZmodP (ZmodP, ZmodP, Integer) deriving Show

instance Arbitrary BSGSZmodP where
    arbitrary = do (Prime p) <- arbitrary :: Gen Prime
                   a <- chooseInteger (1, (p - 1))
                   b <- chooseInteger (1, (p - 1))
                   return $ BSGSZmodP (ZmodP a p, ZmodP b p, gorder (ZmodP a p))
                   

-- ZmodP
prop_ZmodPCommutativeMul :: Integer -> Integer -> Prime -> Bool
prop_ZmodPCommutativeMul x y (Prime p) =
       (ZmodP (x `mod` p) p) `gcompose` (ZmodP (y `mod` p) p) == (ZmodP (y `mod` p) p) `gcompose` (ZmodP (x `mod` p) p)

prop_ZmodPAssociativeMul :: Integer -> Integer -> Integer -> Prime -> Bool
prop_ZmodPAssociativeMul x y z (Prime p) =
          ((ZmodP (x `mod` p) p) `gcompose` (ZmodP (y `mod` p) p)) `gcompose` (ZmodP (z `mod` p) p) ==
           (ZmodP (y `mod` p) p) `gcompose` ((ZmodP (x `mod` p) p) `gcompose` (ZmodP (z `mod` p) p))

prop_ZmodPInverse :: Integer -> Prime -> Bool
prop_ZmodPInverse x (Prime p) =
    let n = ZmodP (x `mod` p) p in
        (x `mod` p) == 0 ||
        (ginv n) `gcompose` n == gid n &&
        n `gcompose` (ginv n) == gid n

prop_ZmodPInversePow :: Integer -> Prime -> Bool
prop_ZmodPInversePow x (Prime p) =
    (ginv (ZmodP (x `mod` p) p)) == (gpow (ZmodP (x `mod` p) p) (-1))

prop_ZmodPFermatLittle :: Integer -> Prime -> Bool
prop_ZmodPFermatLittle x (Prime p) =
    let n = (ZmodP (x `mod` p) p) in
    (gpow n p) == n

prop_ZmodPIdentity :: Integer -> Prime -> Bool
prop_ZmodPIdentity x (Prime p) =
    let n = ZmodP (x `mod` p) p in
        (gid n) `gcompose` n == n &&
        n `gcompose` (gid n) == n


-- ZmodN
prop_ZmodNCommutativeAdd :: Integer -> Integer -> Positive Integer -> Bool
prop_ZmodNCommutativeAdd x y (Positive n) =
       (ZmodN (x `mod` n) n) `gcompose` (ZmodN (y `mod` n) n) == (ZmodN (y `mod` n) n) `gcompose` (ZmodN (x `mod` n) n)

prop_ZmodNAssociativeAdd :: Integer -> Integer -> Integer -> Positive Integer -> Bool
prop_ZmodNAssociativeAdd x y z (Positive n) =
          ((ZmodN (x `mod` n) n) `gcompose` (ZmodN (y `mod` n) n)) `gcompose` (ZmodN (z `mod` n) n) ==
           (ZmodN (y `mod` n) n) `gcompose` ((ZmodN (x `mod` n) n) `gcompose` (ZmodN (z `mod` n) n))

prop_ZmodNInverse :: Integer -> Positive Integer -> Bool
prop_ZmodNInverse x (Positive n) =
    let a = ZmodN (x `mod` n) n in
        (ginv a) `gcompose` a == gid a &&
        a `gcompose` (ginv a) == gid a

prop_ZmodNInversePow :: Integer -> Positive Integer -> Bool
prop_ZmodNInversePow x (Positive n) =
    (ginv (ZmodN (x `mod` n) n)) == (gpow (ZmodN (x `mod` n) n) (-1))

prop_ZmodNIdentity :: Integer -> Positive Integer -> Bool
prop_ZmodNIdentity x (Positive n) =
    let a = ZmodN (x `mod` n) n in
        (gid a) `gcompose` a == a &&
        a `gcompose` (gid a) == a


-- Z
prop_ZCommutativeAdd :: Integer -> Integer -> Bool
prop_ZCommutativeAdd x y =
    (Z x) `radd` (Z y) == (Z y) `radd` (Z x)

prop_ZAssociativeAdd :: Integer -> Integer -> Integer -> Bool
prop_ZAssociativeAdd x y z =
    ((Z x) `radd` (Z y)) `radd` (Z z) == (Z x) `radd` ((Z y) `radd` (Z z))

prop_ZCommutativeMul :: Integer -> Integer -> Bool
prop_ZCommutativeMul x y =
    (Z x) `rmul` (Z y) == (Z y) `rmul` (Z x)

prop_ZAssociativeMul :: Integer -> Integer -> Integer -> Bool
prop_ZAssociativeMul x y z =
    ((Z x) `rmul` (Z y)) `rmul` (Z z) == (Z x) `rmul` ((Z y) `rmul` (Z z))

prop_ZInverse :: Integer -> Bool
prop_ZInverse x =
    let n = Z x in
        case (rinv n) of
          Nothing -> True
          Just inv -> inv `rmul` n == (rid n) && n `rmul` inv == (rid n)

prop_ZInversePow :: Integer -> Bool
prop_ZInversePow x =
    let n = Z x in
        rinv n == rpow n (-1)

prop_ZIdentity :: Integer -> Bool
prop_ZIdentity x =
    let n = Z x in
        (rid n) `rmul` n == n &&
        n `rmul` (rid n) == n

prop_bsgsZmodP :: BSGSZmodP -> Bool
prop_bsgsZmodP (BSGSZmodP (g@(ZmodP _ p), h, order)) =
    let res = bsgs g h order in
        if isNothing res
           then (if order /= p then True else False)
           else (gpow g (fromJust res)) == h

prop_bsgsZmodN :: BSGSZmodN -> Bool
prop_bsgsZmodN (BSGSZmodN (g@(ZmodN _ n), h, order)) =
    let res = bsgs g h order in
        if isNothing res
           then (if order /= n then True else False)
           else (gpow g (fromJust res)) == h



return []
runTests :: IO Bool
runTests = $forAllProperties $
    quickCheckWithResult (stdArgs {maxSuccess = 10000})
