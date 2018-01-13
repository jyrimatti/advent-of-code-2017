module Day01 where
  
import Data.Char (digitToInt)

input = readFile "input/input01.txt"

calculate = sum . fmap digitToInt . fmap fst . filter (uncurry (==))

pairs toSkip inp = zip inp $ drop toSkip (cycle inp)

solve1     = calculate . pairs 1
solve2 inp = calculate $ pairs (length inp `div` 2) inp

solution1 = solve1 <$> input
solution2 = solve2 <$> input
