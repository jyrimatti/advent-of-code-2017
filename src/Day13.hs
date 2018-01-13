module Day13 where

import Data.List (find)
import Data.Maybe (fromJust)
import Text.Regex.Applicative
import Text.Regex.Applicative.Common

data Layer = Layer {
  depth :: Int,
  range :: Int
}

input      = lines <$> readFile "input/input13.txt"
input_test = lines <$> readFile "input/input13_test.txt"

layerP = fromJust . match (Layer <$> decimal <* string ": " <*> decimal)

hasScanner (Layer _ r) time = time `mod` ((r - 1) * 2) == 0

severity (Layer d r) = d * r

timeAt delay (Layer d _) = d + delay

caught delay layer = hasScanner layer $ timeAt delay layer

tripSeverity delay = sum . fmap severity . filter (caught delay)

solve1 = tripSeverity 0 . fmap layerP

solve2 inp = let
  layers = fmap layerP inp
 in
  fromJust $ find (\delay -> not $ any (caught delay) layers) [0..]

solution1 = solve1 <$> input
solution2 = solve2 <$> input