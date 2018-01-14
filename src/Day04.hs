module Day04 where

import Data.List (permutations)
import Data.Set (intersection, delete)
import qualified Data.Set as Set (fromList)

input = lines <$> readFile "input/input04.txt"

isValid str = let 
  ws = words str
  distinct = Set.fromList ws
 in
  length ws == length distinct
 
solve f = length . filter f

isValid2 str = let
  distinct = Set.fromList $ words str
  anagrams s = delete s . Set.fromList . permutations $ s
  valid = null . intersection distinct . anagrams
 in
  isValid str && all valid distinct
  
solution1 = solve isValid <$> input 
solution2 = solve isValid2 <$> input 