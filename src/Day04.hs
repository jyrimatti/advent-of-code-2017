module Day04 where
  
import Data.Set (fromList, intersection, delete)
import Data.List (permutations)

input = lines <$> readFile "input/input04.txt"

isValid str = let 
  ws = words str
  distinct = fromList ws
 in
  length ws == length distinct
 
solve f = length . filter f

isValid2 str = let
  distinct = fromList $ words str
  anagrams s = delete s $ fromList $ permutations s
  valid = null . intersection distinct . anagrams
 in
  isValid str && all valid distinct
  
solution1 = solve isValid <$> input 
solution2 = solve isValid2 <$> input 