module Crypto.Util.List where

import qualified Data.Map as Map

tr :: Ord a => b -> [a] -> [b] -> [a] -> [b]
tr defaultVal set1 set2 source =
  let replacements = Map.fromList $ zip set1 set2
   in map (\x -> Map.findWithDefault defaultVal x replacements) source
