{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Crypto.Algebra.Ring.Class where

import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe


    {-
        TYPECLASS DEFINITIONS
    -}

class (Eq a) => Ring a where
    rzero :: a -> a
    radd, rmul :: a -> a -> a
    rneg :: a -> a
    rpow :: (Integral i) => a -> i -> Maybe a
    rinv :: a -> Maybe a

class (Ring a) => IdentityRing a where
    rid :: a -> a -- Use with rid :: Z or something

class (Ring a) => CommutativeRing a where


    {-
        DATA TYPES AND INSTANCE DEFINITIONS
    -}

    {-

-- |Polynomial ring R[x] with ring elements x.
newtype Rx a = Rx [a] deriving (Read, Show, Eq)

readZx :: String -> Rx Z
readZx input =
    let readterm (sign, input) =
            let f = if sign == "-" then rneg else id
                (c1, t1) = head (lex input)
                (c2, t2) = head (lex t1)
                (c3, t3) = head (lex t2)
                (c4, t4) = head (lex t3)
                (c5, t5) = head (lex t4)
             in case () of _
                             -- a*x^e
                             | c2 == "*" && c4 == "^"
                                -> (f $ Z (read c1), read c5 :: Integer)
                             -- ax^e
                             | c3 == "^"
                                -> (f $ Z (read c1), read c4 :: Integer)
                             -- x^e
                             | c2 == "^"
                                -> (f $ Z 1, read c3 :: Integer)
                             -- a*x
                             | c2 == "*" && all isAlpha c3
                                -> (f $ Z (read c1), 1)
                             -- ax
                             | all isAlpha c2 && not (null c2)
                                -> (f $ Z (read c1), 1)
                             -- x
                             | all isAlpha c1 && c2 == ""
                                -> (f $ Z 1, 1)
                             -- a
                             | otherwise
                                -> (f $ Z (read c1), 0)
        collected []       = []
        collected [_]      = error "Splitinput not working correctly: odd number of elements given to collected"
        collected (x:y:xs) = (x, y) : collected xs
        isLeadingTermNegative = fst (head (lex input)) == "-"
        splitinput = if isLeadingTermNegative then tail (split (oneOf "+-") input) else "+" : split (oneOf "+-") input
        terms = collected splitinput
        sorted = sortOn snd $ map readterm terms
        converted _ [] = []
        converted i t@((a, e) : xs)
          | i == e    = a : converted (i + 1) xs
          | otherwise = Z 0 : converted (i + 1) t
     in Rx (converted 0 sorted)

-- |Multiply polynomial by a scalar
scalarmul :: (Ring a) => a -> Rx a -> Rx a
scalarmul a (Rx b) = Rx (map (rmul a) b)

-- |Match the length of the two ring element arrays, with the shorter
-- one being padded to the right with the zero element
matchlength :: (Ring a) => [a] -> [a] -> ([a], [a])
matchlength [] [] = ([],[])
matchlength a@[] b@(x:_) = 
    let lena = length a
        lenb = length b
        maxlen = max lena lenb
     in (a ++ replicate (maxlen - lena) (rzero x), b ++ replicate (maxlen - lenb) (rzero x))
matchlength a@(x:_) b =
    let lena = length a
        lenb = length b
        maxlen = max lena lenb
     in (a ++ replicate (maxlen - lena) (rzero x), b ++ replicate (maxlen - lenb) (rzero x))

instance IdentityRing a => IdentityRing (Rx a) where
    rid (Rx a) = Rx [rid (head a)]

instance IdentityRing a => Ring (Rx a) where
    rzero (Rx xs) = Rx []
    radd (Rx a') (Rx b') =
        let (a, b) = matchlength a' b'
        in Rx (zipWith radd a b)
    rmul (Rx a) (Rx b) =
        let a' = zip a [0..]
         in foldl' (\acc (x, i) -> radd acc (scalarmul x (Rx (replicate i (rzero x) ++ b)))) (Rx []) a'
    rneg (Rx a) = Rx (map rneg a)
    rpow b e
      | e >= 0 = Just $ rxpow b e
      | otherwise = 
          let inv = rinv b
           in if isNothing inv then Nothing else Just $ rxpow (fromJust inv) (-e)
    rinv (Rx (x:xs)) =
        if null xs
           then do inv <- rinv x
                   return (Rx [inv])
           else Nothing
    rinv _ = Nothing

rxpow :: (IdentityRing a, Integral b) => Rx a -> b -> Rx a
rxpow b 0 = rid b
rxpow b 1 = b
rxpow b e =
    let square x = rmul x x
     in if even e
           then square (rxpow b (e `div` 2))
           else rmul b (rxpow b (e - 1))
           -}
