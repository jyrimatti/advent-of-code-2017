module Day05 where
  
import Prelude hiding (length)
import Data.Sequence (index,adjust,length,fromList)

input = fmap read <$> lines <$> readFile "input/input05.txt"

modifyOffset1 = (+) 1

modifyOffset2 x | x >= 3 = x - 1
modifyOffset2 x          = modifyOffset1 x

step _              stepnum _    i | i < 0            = stepnum
step _              stepnum sequ i | i >= length sequ = stepnum
step offsetModifier stepnum sequ i = step offsetModifier (stepnum+1) (adjust offsetModifier i sequ) (i + sequ `index` i)

solve f list = step f (0::Int) (fromList list) 0

solve1 = solve modifyOffset1
solve2 = solve modifyOffset2

solution1 = solve1 <$> input
solution2 = solve2 <$> input