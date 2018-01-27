module Day17 where

import Prelude hiding (splitAt)
import Data.Maybe (fromJust)

import Data.Sequence (singleton,index,elemIndexL,splitAt,(><),(<|))

input = 371

step iterations buffer _   _       newValue | newValue == (iterations+1) = buffer
step iterations buffer inp current newValue = let
  newPosition     = (current + inp) `mod` length buffer + 1
  (prefix,suffix) = splitAt newPosition buffer
 in
  step iterations (prefix >< (newValue <| suffix)) inp newPosition $! newValue + 1

solve endValue iterations inp = let
  completedBuffer = step iterations (singleton 0) inp 0 (1 :: Int) 
 in
  completedBuffer `index` (1 + fromJust (elemIndexL endValue completedBuffer))

-- "What is the value after 2017"
solution1 = solve 2017 2017 input

-- "What is the value after 0"
solution2 = solve 0 50000000 input