module Day03 where
  
import Data.Maybe (fromJust)
import Data.Foldable

input = 361527

squaresOnLevel level = 4 * (level * 2 - 1 + 1)

totalSquares 0 = 1
totalSquares level = totalSquares (level - 1) + squaresOnLevel level

levelOf square = length $ takeWhile (< square) $ fmap totalSquares [0..]

startingDistance level = (-1) * (level - 1)

distanceFromMiddle level i = startingDistance level + (i `mod` (squaresOnLevel level `div` 4))

index square = square - totalSquares (levelOf square - 1) - 1

distance 1 = 0
distance square = let 
  level = levelOf square
 in
  level + (abs . distanceFromMiddle level . index) square

row 1 = 0
row square = let
  level = levelOf square
  foo = squaresOnLevel level `div` 8
 in  
  case startingDistance level + index square of
    i | i <= foo                 -> i
    i | i > foo && i <= foo*3    -> level
    i | i > foo*3 && i <= foo*5  -> (squaresOnLevel level `div` 2) - i
    _                            -> (-1) * level

column 1 = 0
column square = let
  level = levelOf square
  foo = squaresOnLevel level `div` 8
 in  
  case startingDistance level + index square of
    i | i <= foo                -> level
    i | i > foo && i <= foo*3   -> (foo*2) - i
    i | i > foo*3 && i <= foo*5 -> (-1) * level
    i                           -> i - (foo * 6)
    
isNeighbouring square1 square2 = abs (row square1 - row square2) <= 1 && abs (column square1 - column square2) <= 1

sumValue _ 1 = 1
sumValue previous square = sum $ fmap (\s -> snd $ fromJust $ find ((==) s . fst) previous) others
  where others = filter (isNeighbouring square) . takeWhile (\x -> levelOf x >= levelOf square - 1) . reverse $ [1..square-1]

sumValues = reverse . fmap snd . foldr (\x xs -> (x,sumValue xs x) : xs) [] . reverse $ [1..2000]

solve1 = distance
solve2 inp = head $ dropWhile (<= inp) sumValues

solution1 = solve1 input
solution2 = solve2 input