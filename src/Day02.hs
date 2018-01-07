module Day02 where
 
import Data.Bifunctor (bimap)
import Data.Semigroup (Max(..),Min(..))

parse = fmap (fmap read) . fmap words . lines

input = parse <$> readFile "input/input02.txt"
input_test1 = parse <$> readFile "input/input02_test1.txt"
input_test2 = parse <$> readFile "input/input02_test2.txt"

row :: [Int] -> Int
row = uncurry (-) . bimap getMax getMin . foldMap (\v -> (Max v,Min v))
 
row2 inp = do
  x <- inp
  y <- inp
  if (x /= y && mod x y == 0) then return (x `div` y) else return 0

solve1 = sum . fmap row
solve2 = sum . fmap (sum . row2)

solution1 = solve1 <$> input
solution2 = solve2 <$> input