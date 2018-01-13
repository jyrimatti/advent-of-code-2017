module Day05 where
  
import Prelude hiding (length)
import qualified Data.Sequence as Seq (fromList)
import Data.Sequence (index,adjust,length)

input = fmap read <$> lines <$> readFile "input/input05.txt"

modifyOffset1 = (+ 1)

modifyOffset2 x | x >= 3 = x - 1
modifyOffset2 x          = modifyOffset1 x

step _              stepnum i _    | i < 0            = stepnum
step _              stepnum i sequ | i >= length sequ = stepnum
step offsetModifier stepnum i sequ                    = step offsetModifier (stepnum+1) (i + sequ `index` i) (adjust offsetModifier i sequ)

solve offsetModifier = step offsetModifier (0::Int) 0 . Seq.fromList

solve1 = solve modifyOffset1
solve2 = solve modifyOffset2

solution1 = solve1 <$> input
solution2 = solve2 <$> input