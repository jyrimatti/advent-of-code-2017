module Day17 where

import Prelude hiding (length,(!!),splitAt,(++))
import Data.Maybe (fromJust)
import Data.Sequence (singleton,length,index,elemIndexL,splitAt,(><),(<|))

input :: IO Int
input = return 371

step iterations buffer _     _       position | position == (iterations+1) = buffer
step iterations buffer input current position = let
  newPos = ((current + input) `mod` (length buffer)) + 1
  (prefix,suffix) = splitAt newPos buffer
 in step iterations (prefix >< (position <| suffix)) input newPos $! (position + 1)

solve :: Int -> Int -> Int -> Int
solve endValue iterations input = let
  res = step iterations (singleton 0) input 0 1 
 in res `index` (1 + fromJust (elemIndexL endValue res))

solve_test = solve 2017 2017 3

solution1 = solve 2017 2017 <$> input
solution2 = solve 0 50000000 <$> input