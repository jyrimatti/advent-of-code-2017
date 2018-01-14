module Day01 where
  
import Data.Char (digitToInt)

input = fmap digitToInt <$> readFile "input/input01.txt"

-- zips list to pairs with a given offset
pairs offset inp = zip inp $ drop offset (cycle inp)

-- elements that are equal to their pair
equalPairs = fmap fst . filter (uncurry (==))

-- solve with an offset of 1 ("all digits that match the next digit")
solve1     = sum . equalPairs . pairs 1

-- solve with an offset of half-a-list ("the digit halfway around the circular list")
solve2 inp = sum . equalPairs $ pairs (length inp `div` 2) inp

solution1 = solve1 <$> input
solution2 = solve2 <$> input
