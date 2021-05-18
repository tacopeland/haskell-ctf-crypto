{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Crypto.Ring where

import Data.List
import Data.Maybe

class Ring a where
    rzero :: a -> a
    radd, rmul :: a -> a -> a
    rneg :: a -> a
    rpow :: (Integral i) => a -> i -> Maybe a
    rinv :: a -> Maybe a

class (Ring a) => IdentityRing a where
    rid :: a -> a -- Use with rid :: Z or something

class (Ring a) => CommutativeRing a where

-- Integer ring
data Z = Z Integer
    deriving (Show, Eq)

instance Ring Z where
    rzero _ = (Z 0)
    radd (Z a) (Z b) = Z (a + b)
    rmul (Z a) (Z b) = Z (a * b)
    rneg (Z a) = (Z (-a))
    rpow (Z a) b
      | a == -1 = Just $ Z (if (even b) then 1 else (-1))
      | a == 1 && b == -1 = Just $ Z 1
      | (b >= 0) = Just (Z (a ^ b))
      | otherwise = Nothing
    rinv (Z a)       = if (a == -1) || (a == 1) then Just (Z a) else Nothing

instance IdentityRing Z where
    rid _            = (Z 1)

instance CommutativeRing Z where


-- Polynomial ring with ring elements
data Rx a = Rx [a] deriving (Show, Eq)

scalarmul :: (Ring a) => a -> Rx a -> Rx a
scalarmul a (Rx b) = Rx (map (rmul a) b)

matchlength :: (Ring a) => [a] -> [a] -> ([a], [a])
matchlength [] [] = ([],[])
matchlength a@[] b@(x:_) = 
    let lena = length a
        lenb = length b
        maxlen = max (lena) (lenb)
     in (a ++ (take (maxlen - lena) (repeat (rzero x))), b ++ (take (maxlen - lenb) (repeat (rzero x))))
matchlength a@(x:_) b =
    let lena = length a
        lenb = length b
        maxlen = max (lena) (lenb)
     in (a ++ (take (maxlen - lena) (repeat (rzero x))), b ++ (take (maxlen - lenb) (repeat (rzero x))))

instance IdentityRing a => IdentityRing (Rx a) where
    rid (Rx a) = Rx ([rid (head a)])


instance IdentityRing a => Ring (Rx a) where
    rzero (Rx r@(x:xs)) = Rx (take (length r) $ repeat (rzero x))
    radd (Rx a') (Rx b') =
        let (a, b) = matchlength a' b'
        in Rx (zipWith radd a b)
    rmul (Rx a) (Rx b) =
        let a' = zip a [0..]
         in foldl (\acc (x, i) -> radd acc (scalarmul x (Rx ((take i $ repeat (rzero x)) ++ b)))) (Rx []) a'
    rneg (Rx a) =
        Rx (map (\x -> rneg x) a)
    rpow b e
      | e >= 0 = Just $ rxpow b e
      | otherwise = 
          let inv = rinv b
           in if isNothing inv then Nothing else Just $ rxpow (fromJust inv) (-e)
    rinv (Rx (x:xs)) =
        if length xs == 0
           then do inv <- (rinv x)
                   return $ Rx [inv]
           else Nothing


rxpow :: (IdentityRing a, Integral b) => Rx a -> b -> Rx a
rxpow b 0 = 
    rid b
rxpow b e =
    let square x = rmul x x
     in if even e
           then square (rxpow b (e `div` 2))
           else rmul b (rxpow b (e - 1))
