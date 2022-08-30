module Internal.Helpers where

import Data.Bits ((.&.))
import Data.List
import qualified Data.Map as Map
import Data.Maybe


    {-
       Binary and mathematical
    -}

-- |Generates a list of squares given a multiplication function.
squares :: (a -> a -> a) -> a -> [a]
squares mult = iterate (\x -> x `mult` x)

-- |Expand an integer into its binary representation.
binexpand :: (Integral a) => a -> [a]
binexpand 0 = []
binexpand a 
  | even a    = 0 : leftover
  | otherwise = 1 : leftover
  where leftover = binexpand $ a `div` 2

nDifferences :: Eq a => [a] -> [a] -> Int
nDifferences [] _ = 0
nDifferences _ [] = 0
nDifferences (x:xs) (y:ys) = if x /= y then 1 + nDifferences xs ys else nDifferences xs ys

bitlen :: (Integral a) => a -> Int
bitlen = length . binexpand

-- |Turn a number of the form n into a*2^e. 
decomposeEven :: (Integral a) => a -> (a, a)
decomposeEven n = inner (n, 0)
    where inner (a, e)
            | even a && a > 0 = inner (a `div` 2, e + 1)
            | otherwise       = (a, e)

-- |http://www.graphics.stanford.edu/~seander/bithacks.html#DetermineIfPowerOf2
powerOf2 :: Integer -> Bool
powerOf2 a = a .&. (a - 1) == 0

nextPowerOf2 :: Integer -> Integer
nextPowerOf2 a
  | a <= 1    = 1
  | otherwise = 2 ^ (floor (logBase 2 (fromIntegral (a-1))) + 1)

consecutivePowers :: Integer -> [Integer]
consecutivePowers k = iterate (*k) 1

consecutiveModPowers :: Integer -> Integer -> [Integer]
consecutiveModPowers k n = iterate ((`mod` n) . (*k)) 1


    {-
       Generic algorithms
    -}

-- |Generic square-and-multiply algorithm.
squareAndMultiply :: (Integral b) => (a -> a -> a) -> a -> a -> b -> a
squareAndMultiply operation identity base exponent =
    foldr operation identity
        (zipWith (\a e -> if e == 1 then a else identity)
            (squares operation base)
            (binexpand exponent))

-- |Generic collision algorithm. Expects that both lists are of the
-- same size and contain no duplicates.
collide :: (Eq a) => [a] -> [a] -> [a] -> Int -> Maybe (a, Int, Int)
collide (x:xs) (y:ys) l index
  | x == y     = Just (x, index, index)
  | x `elem` l = Just (x, index, getInd x `div` 2)
  | y `elem` l = Just (y, getInd y `div` 2, index)
  | otherwise  = collide xs ys (x:y:l) (index + 1)
  where getInd = (-) (length l - 1) . fromJust . flip elemIndex l
collide _ _ _ _ = Nothing


-- |Get frequency analysis of a list.
freqTable :: (Ord a) => [a] -> [(a, Integer)]
freqTable l = Map.toList (Map.fromListWith (+) [(c, 1) | c <- l])

-- |Get local maxima of a list.
localMaxima :: (Ord a) => [a] -> [a]
localMaxima [] = []
localMaxima [x] = [x]
localMaxima [x, y]
  | y > x = [y]
  | otherwise = []
localMaxima (x:y:z:rest)
  | y > x && y > z = y : localMaxima (y:z:rest)
  | otherwise = localMaxima (y:z:rest)

substitute :: (Ord a, Eq a) => [a] -> Map.Map a a -> [a]
substitute toSub subTable =
    map (\e -> Map.findWithDefault e e subTable) toSub


-- |The newState function picks a new state from the current state.
-- If the new state's fitness isn't better than the current one's, then
-- tell the state that this guess should be skipped on next iteration
-- with the nextGuess function.
hillClimb :: (Ord b) => a -> (a -> b) -> (a -> Bool) -> (a -> a) -> (a -> a) -> a
hillClimb state fitness isGoal newState nextGuess
  | isGoal state = state
  | fitness state' > fitness state =
      hillClimb state' fitness isGoal newState nextGuess
  | otherwise = hillClimb (nextGuess state) fitness isGoal newState nextGuess
  where 
    state' = newState state
