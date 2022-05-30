{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
module Algebra.Structure.Ring where

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
    rid :: a -> a

class (Ring a) => CommutativeRing a where

class (Ring a) => FiniteRing a where
    rcardinality :: a -> Integer
    -- |Multiplicative order of the first argument in its ring.
    rorder :: a -> Integer

class (FiniteRing a, Ring a, Ring b) => QuotientRing a b | a -> b where
    qrelement :: a -> b
    qrideal :: a -> b
    qrcoerce :: b -> b -> a


instance Ring Integer where
    rzero _ = 0
    radd = (+)
    rmul = (*)
    rneg x = -x
    rpow x e
      | e >= 0    = Just (x ^ e)
      | otherwise = Nothing
    rinv x
      | x == 1    = Just 1
      | otherwise = Nothing

instance IdentityRing Integer where
    rid _ = 1


        {-
-- |Quotient ring Rx/(m)
data Rxm a = Rxm { rxm_element :: Rx a, rxm_ideal :: Rx a}
    deriving (Read, Show, Eq)

    {-
instance (Show a) => Show (Rxm a) where
    show (Rxm a m) = show a ++ " mod " ++ show m

instance (Read a) => Read (Rxm a) where
    readsPrec i input =
        let ((a, tail1):xs) = readsPrec (i+1) input
            ((txt, tail2):ys) = readsPrec (i+1) tail1
            ((n, tail3):zs) = readsPrec (i+1) tail2
         in if txt == "mod" then [(Rxm a n, tail3)] else []
         -}

instance (EuclideanDomain a) => QuotientRing (Rxm a) (Rx a) where
    qrelement a  = rxm_element a
    qrideal a    = rxm_ideal a
    qrcoerce a n = Rxm (a `modulo` n) n

instance (EuclideanDomain a) => Ring (Rxm a) where
    rzero (Rxm a n)  = Rxm (rzero a) n
    radd (Rxm a n) (Rxm b n')
        | n == n'    = qrcoerce (radd a b) n
        | otherwise  = error "Trying to add two quotient ring elements with different ideals in radd"
    rmul (Rxm a n) (Rxm b n')
        | n == n'    = qrcoerce (rmul a b) n
        | otherwise  = error "Trying to multiply two quotient ring elements with different ideals in rmul"
    rneg (Rxm a n)   = qrcoerce (rneg a) n
    rpow             = rxmpow
    rinv                   = znzModinv

rx_modmul :: (EuclideanDomain a) => Rx a -> Rx a -> Rx a
rx_modmul a b n =
    let double x = modulo (x `radd` x) n
        identity = rid a
        zero     = rzero a
     in if b == zero
           then identity
           else if even 

rxmpow :: (EuclideanDomain a, Integral b) => Rxm a -> b -> Maybe (Rxm a)
rxmpow b e
    | e == 0            = Just identity
    | e >= 0            = foldl (\acc x -> rmul <$> acc <*> x) (Just identity)
                          (zipWith (\(Rxm b n) e -> Just (qrcoerce (fromJust $ rpow b e) n))
                              (modsquares b)
                              (binexpand e))
    | invmod == Nothing = Nothing
    | otherwise         = rxmpow new_n (-e)
    where identity = rid b
          invmod = rinv b
          Just new_n = invmod


rx_modpow :: (Field a, Integral b) => Rx a -> b -> Rx a -> Rx a
rx_modpow b 0 n = rid b
rx_modpow b e n =
    let square x = rmul x x
     in if even e
           then modulo (square (rx_modpow b (e `div` 2) n)) n
           else modulo (rmul b (rx_modpow b (e - 1) n)) n
           -}

    {-
-- |Horner's rule
evaluatePoly (Rx a) x =
    foldl' (\acc c -> radd (rmul acc x) c) (rzero (head a)) (reverse a)

instance (EuclideanDomain a) => IdentityRing (Rxm a) where
    rid (Rxm a n) = Rxm (rid a) n


    {-
        MISCELLANEOUS FUNCTIONS
    -}

{- (This doesn't work yet, need factorization routines)
isPrimitiveRoot :: ZnZ -> Bool 
isPrimitiveRoot x@(ZnZ (Z a) (Z n)) =
    coprime a n && rpow x (n - 1) == Just (rid x)
    -}

isqrt = toInteger . floor . sqrt . fromIntegral

isPrime k = k > 1 && null [ x | x <- [2..isqrt k], k `mod` x == 0]

ntt' :: (IdentityRing a) => [a] -> a -> [a]
ntt' a' g =
    let m = ceiling (logBase 2 (fromIntegral $ length a'))
        n = 2 ^ m
        a = a' ++ replicate (n - length a') (rzero g)
        (a0, a1) = foldr (\x (xs,ys) -> (x:ys,xs)) ([],[]) a
        Just inv2 = rinv (radd (rid g) (rid g))
        y0 = ntt' a0 (rmul inv2 g)
        y1 = ntt' a1 (rmul inv2 g)
        omegas = iterate (rmul g) (rid g)
        omegay1 = zipWith rmul y1 omegas
        ys = zipWith (\x y -> (radd x y, radd x (rneg y))) y0 omegay1
        y = uncurry (++) (unzip ys)
     in if length a' == 1 then a' else y

ntt a' g =
    let a = a' ++ replicate (2 * length a') (rzero g)
     in ntt' a g

invntt' :: (Integral a) => [ZnZ] -> ZnZ -> a -> [ZnZ]
invntt' y' g l =
    let m = ceiling (logBase 2 (fromIntegral $ length y'))
        n = 2 ^ m
        y = y' ++ replicate (n - length y') (rzero g)
        (y0, y1) = foldr (\x (xs,ys) -> (x:ys,xs)) ([],[]) y
        a0 = invntt' y0 g (l + 1)
        a1 = invntt' y1 g (l + 1)
        omegas = map (fromJust . rinv) (drop (n `div` 2) (iterate (rmul g) (rid g)))
        Just i = rpow (radd (rid g) (rid g)) l >>= rinv
        as = zipWith3 (\x y omega -> (rmul i (radd x y), rmul i (rmul omega (radd x (rneg y))))) a0 a1 omegas
        a = uncurry (++) (unzip as)
     in if length y' == 1 then y' else a

invntt y g =
    let Just ginv = rinv g
        Just inv = rinv (ZnZ (Z (toInteger $ length y)) (qrideal g))
     in map (rmul inv) (invntt' y ginv 1)

rxmodadd :: (EuclideanDomain a) => Rx a -> Rx a -> Rx a -> Rx a
rxmodadd a b = modulo (radd a b)

rxmodmul :: (EuclideanDomain a) => Rx a -> Rx a -> Rx a -> Rx a
rxmodmul (Rx a) (Rx b) n =
    let a' = zip a [0..]
     in foldl' (\acc (x, i) -> radd acc (scalarmul x (Rx (replicate i (rzero x) ++ b)))) (Rx []) a'
 



-}

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
