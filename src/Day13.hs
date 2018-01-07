{-# LANGUAGE OverloadedStrings #-}
module Day13 where

import Data.Text (splitOn,pack,unpack)
import Data.List (find)
import Data.Maybe (fromJust)

data Layer = Layer {
  depth :: Int,
  range :: Int
}

input = lines <$> readFile "input/input13.txt"
input_test = lines <$> readFile "input/input13_test.txt"

layers = fmap (\[a,b] -> Layer a b) . fmap (fmap (read . unpack) <$> splitOn ":" . pack)

hasScanner (Layer _ range) time = time `mod` ((range - 1) * 2) == 0

severity (Layer depth range) = depth * range

timeAt delay (Layer depth _) = depth + delay

caught delay layer = hasScanner layer $ timeAt delay layer

tripSeverity delay = sum . fmap severity . filter (caught delay)

solve1 = tripSeverity 0 . layers

solve2 input = let
  ls = layers input
 in
  fromJust $ find (\delay -> not $ any (caught delay) ls) [0..]

solution1 = solve1 <$> input
solution2 = solve2 <$> input