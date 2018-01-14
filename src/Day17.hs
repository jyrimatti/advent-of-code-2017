module Day17 where

import Prelude hiding (splitAt)
import Data.Maybe (fromJust)

import Data.Sequence (singleton,index,elemIndexL,splitAt,(><),(<|))

input = 371

step iterations buffer _   _       position | position == (iterations+1) = buffer
step iterations buffer inp current position = let
  newPos          = (current + inp) `mod` length buffer + 1
  (prefix,suffix) = splitAt newPos buffer
 in step iterations (prefix >< (position <| suffix)) inp newPos $! position + 1

solve endValue iterations inp = let
  res = step iterations (singleton 0) inp 0 (1 :: Int) 
 in res `index` (1 + fromJust (elemIndexL endValue res))

solve_test = solve 2017 2017 3

solution1 = solve 2017 2017 input
solution2 = solve 0 50000000 input