{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Crypto.Ring where

import Data.List
import Data.List.Split
import Data.Maybe


    {-
        TYPECLASS DEFINITIONS
    -}

class Ring a where
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

-- |Integer ring Z
data Z = Z Integer
    deriving (Eq)

instance Show Z where
    show (Z a) = show a

instance Read Z where
    readsPrec _ input =
        let (a, t) = head (lex input)
         in [(Z (read a), t)]

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


-- |Polynomial ring R[x] with ring elements x.
data Rx a = Rx [a] deriving (Read, Show, Eq)

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
                             | c2 == "*" && c4 == "^" -> (f $ Z (read c1), read c5 :: Integer)
                             -- ax^e
                             | c3 == "^"              -> (f $ Z (read c1), read c4 :: Integer)
                             -- x^e
                             | c2 == "^"              -> (f $ Z 1, read c3 :: Integer)
                             -- ax
                             | c2 == "x"              -> (f $ Z (read c1), 1)
                             -- x
                             | c1 == "x" && c2 == ""  -> (f $ Z 1, 1)
                             -- a
                             | otherwise              -> (f $ Z (read c1), 0)
        collected [] = []
        collected (x:y:xs) = (x, y) : collected xs
        isLeadingTermNegative = if fst (head (lex input)) == "-" then True else False
        splitinput = if isLeadingTermNegative then (tail (split (oneOf "+-") input)) else ("+" : (split (oneOf "+-") input))
        terms = collected splitinput
        sorted = sortOn snd $ map readterm terms
        converted _ [] = []
        converted i t@((a, e) : xs)
          | i == e    = a : converted (i + 1) xs
          | otherwise = (Z 0) : converted (i + 1) t
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
    rneg (Rx a) = Rx (map (\x -> rneg x) a)
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
rxpow b 0 = rid b
rxpow b e =
    let square x = rmul x x
     in if even e
           then square (rxpow b (e `div` 2))
           else rmul b (rxpow b (e - 1))
