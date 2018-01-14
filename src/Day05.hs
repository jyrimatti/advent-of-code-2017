module Day05 where

import qualified Data.Sequence as Seq (fromList)
import Data.Sequence (index,adjust)

input = fmap read <$> lines <$> readFile "input/input05.txt"

modifyOffset1 = (+ 1)

modifyOffset2 x | x >= 3 = x - 1
modifyOffset2 x          = modifyOffset1 x

step _              steps instruction _       | instruction < 0               = steps
step _              steps instruction program | instruction >= length program = steps
step offsetModifier steps instruction program                                 = step offsetModifier
                                                                                     (steps+1)
                                                                                     (instruction + program `index` instruction)
                                                                                     (adjust offsetModifier instruction program)

-- solve with a given modification function for current offset
solve offsetModifier = step offsetModifier (0::Int) 0 . Seq.fromList

solve1 = solve modifyOffset1
solve2 = solve modifyOffset2

solution1 = solve1 <$> input
solution2 = solve2 <$> input