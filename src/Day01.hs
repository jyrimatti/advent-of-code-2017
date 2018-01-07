module Day01 where
  
import Data.Char (digitToInt)

input = readFile "input/input01.txt"

pairs inp = zip inp (tail inp ++ [head inp])

calculate = sum . map digitToInt . map fst . filter (uncurry (==))

pairs2 inp = zip inp (drop (length inp `div` 2) inp ++ inp)

solution1 = calculate <$> pairs <$> input
solution2 = calculate <$> pairs2 <$> input
