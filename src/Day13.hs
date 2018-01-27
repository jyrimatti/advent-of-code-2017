module Day13 where

import Data.Maybe (fromJust)
import Data.List (find)

import Text.Regex.Applicative
import Text.Regex.Applicative.Common

data Layer = Layer {
  _depth :: Int,
  _range :: Int
}

input      = lines <$> readFile "input/input13.txt"
input_test = lines <$> readFile "input/input13_test.txt"

layerP = fromJust . match (Layer <$> decimal <* string ": " <*> decimal)

-- takes 2*(range-1) time steps for the scanner to get back to the top
hasScanner (Layer _ range) time = time `mod` ((range - 1) * 2) == 0

severity (Layer depth range) = depth * range

timeWithDelay delay (Layer depth _) = depth + delay

-- check if scanner is at the given depth after given delay 
caughtAtLayer delay layer@(Layer depth _) = hasScanner layer $ delay + depth

tripSeverity delay = sum . fmap severity . filter (caughtAtLayer delay)

-- "what is the severity of your whole trip"
solve1 = tripSeverity 0 . fmap layerP

-- "fewest number of picoseconds that you need to delay"
solve2 inp = let
  layers = fmap layerP inp
 in
  fromJust $ find (\delay -> not $ any (caughtAtLayer delay) layers) [0..]

solution1 = solve1 <$> input
solution2 = solve2 <$> input