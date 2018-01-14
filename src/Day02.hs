module Day02 where
 
import Data.Bifunctor (bimap)
import Data.Semigroup (Max(..),Min(..))
import Data.Tuple.Extra ((&&&))

parseSpreadsheet = fmap (fmap read) . fmap words . lines

input       = parseSpreadsheet <$> readFile "input/input02.txt"
input_test1 = parseSpreadsheet <$> readFile "input/input02_test1.txt"
input_test2 = parseSpreadsheet <$> readFile "input/input02_test2.txt"

-- difference between maximum and minimum value of a row
rowDifference :: [Int] -> Int
rowDifference = uncurry (-) . bimap getMax getMin . foldMap (Max &&& Min)

-- division of row numbers that are evenly divisible by each other
rowDivision inp = sum $ do
  a <- inp
  b <- filter (/= a) $ inp
  return $ case a `mod` b of
    0 -> a `div` b
    _ -> 0

solve1 = sum . fmap rowDifference
solve2 = sum . fmap rowDivision

solution1 = solve1 <$> input
solution2 = solve2 <$> input