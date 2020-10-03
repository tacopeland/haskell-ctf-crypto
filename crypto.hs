import Data.List
import Data.Maybe


data ZmodN = ZmodN Integer Integer
    deriving (Show, Eq)

instance Num ZmodN where
    (+) (ZmodN a n) (ZmodN b _) = ZmodN ((a + b) `mod` n) n
    (-) (ZmodN a n) (ZmodN b _) = ZmodN ((a - b) `mod` n) n
    (*) (ZmodN a n) (ZmodN b _) = ZmodN ((a * b) `mod` n) n
    abs (ZmodN a n)             = ZmodN a n
    signum _                    = 1
    fromInteger n               = ZmodN 0 n
    negate (ZmodN a n)          = ZmodN ((n - a) `mod` n) n


bareInteger :: ZmodN -> Integer
bareInteger (ZmodN a n) = a

modulus :: ZmodN -> Integer
modulus (ZmodN a n) = n

ofIntegral :: (Integral a, Integral b) => a -> b -> ZmodN
ofIntegral a n = ZmodN (toInteger a `mod` toInteger n) (toInteger n)


modsquares :: ZmodN -> [ZmodN]
modsquares n = iterate (\x -> x*x) n

pow :: (Integral a) => ZmodN -> a -> Maybe ZmodN
pow n x
    | x >= 0              = Just $ foldr (*) identity
                                (zipWith (\(ZmodN a mod) e -> ofIntegral (a^e) mod)
                                    (modsquares n)
                                    (binexpand x))
    | invmod == Nothing   = Nothing
    | otherwise           = pow new_n (-x)
    where identity = ZmodN 1 (modulus n)
          invmod = modinv n
          Just new_n = invmod

gcd' :: (Integral a) => a -> a -> a
gcd' a 0 = a
gcd' a b = gcd b (a `mod` b)

xgcd :: (Integral a) => a -> a -> (a,a,a)
xgcd a 0
    | a < 0 = (-a,-1,0)
    | otherwise = (a,1,0)
xgcd a b =
    let (g, t, t2) = xgcd b (a `mod` b)
    in  (g, t2, t - (a `div` b) * t2)

modinv :: ZmodN -> Maybe ZmodN
modinv (ZmodN a m)
    | g /= 1    = Nothing
    | otherwise = Just (ofIntegral x m)
    where (g, x, _) = xgcd a m

binexpand :: (Integral a) => a -> [a]
binexpand 0 = []
binexpand a 
    | a `mod` 2 == 1 = 1 : leftover
    | otherwise      = 0 : leftover
    where leftover = binexpand $ a `div` 2

mulmod :: (Integral a) => [a] -> a -> Maybe ZmodN
mulmod [] _ = Nothing
mulmod (x:[]) m = Just (ofIntegral x m)
mulmod (x:xs) m = Just (ofIntegral (toInteger x * p) m)
    where Just (ZmodN p _) = mulmod xs m

-- |Order (naive)
order :: ZmodN -> Maybe ZmodN
order (ZmodN 0 _) = Nothing
order n           = Just (ofIntegral (toInteger index + 1) (modulus n))
    where Just index = elemIndex identity (iterate (*n) n)
          identity = ofIntegral 1 (modulus n)

babystep :: ZmodN -> [ZmodN]
babystep n = iterate (*n) (ofIntegral 1 (modulus n))

giantstep :: ZmodN -> ZmodN -> [ZmodN]
giantstep h u = iterate (*u) h

-- |Shanks' Baby-Step Giant-Step discrete logarithm algorithm
bsgs :: ZmodN -> ZmodN -> ZmodN -> Maybe ZmodN
bsgs g h ord =
    let 
        n = (floor . sqrt. fromIntegral) (bareInteger ord) + 1
        u = pow g (-n)
        bstep = take n $ babystep g
        gstep
            | u == Nothing = []
            | otherwise    = take n $ giantstep h (fromJust u)
    in case intersect bstep gstep of
        [] -> Nothing
        (match:_) -> flip ofIntegral (bareInteger ord) <$> (toInteger <$> bare_int) 
            where getInd = elemIndex match
                  val = (+) <$> (getInd bstep) <*> ((n*) <$> (getInd gstep))
                  bare_int = flip mod (fromIntegral $ bareInteger ord) <$> val

crt :: [ZmodN] -> Maybe ZmodN
crt []       = Nothing
crt (a:[])   = Just a
crt (a:b:cs) =
    let 
        y = (*) <$> Just (b - (ofIntegral (bareInteger a) (modulus b))) <*> modinv (ofIntegral (modulus a) (modulus b))
    in case y of
        Nothing          -> Nothing
        Just (ZmodN x _) -> crt (ofIntegral (bareInteger a + modulus a * x) (modulus a * modulus b) : cs)
