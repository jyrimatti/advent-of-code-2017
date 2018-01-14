module Day04 where

import Data.List (permutations)
import Data.Set (intersection, delete)
import qualified Data.Set as Set (fromList)

input = lines <$> readFile "input/input04.txt"

-- "a valid passphrase must contain no duplicate words"
isValidPassphrase str = let 
  distinctWords = Set.fromList $ words str
 in
  length (words str) == length distinctWords
 
-- amount of passphrases satisfying a given predicate
solve predicate = length . filter predicate

-- "a valid passphrase must contain no two words that are anagrams of each other"
isValidPassphrase2 str = let
  distinctWords = Set.fromList $ words str
  anagrams s = delete s . Set.fromList . permutations $ s
  valid = null . intersection distinctWords . anagrams
 in
  isValidPassphrase str && all valid distinctWords
  
solution1 = solve isValidPassphrase  <$> input 
solution2 = solve isValidPassphrase2 <$> input 