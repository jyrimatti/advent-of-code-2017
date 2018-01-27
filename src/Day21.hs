module Day21 where

import Data.Bifunctor (bimap)
import Data.Maybe (fromJust)
import Data.List (find)
import Data.List.Split (chunksOf)
import Data.Matrix.Unboxed (Matrix,fromLists,tr,toList,toLists,rows,subMatrix,fromBlocks,force)

import Text.Parsec (parse,many1)
import Text.Parsec.Char (string,noneOf,char)
import Text.Parsec.Combinator (sepBy1)

parsePattern = fromLists . fmap (fmap onOrOff)

initialImage = parsePattern [".#.","..#","###"]

input = lines <$> readFile "input/input21.txt"

type Pattern = Matrix Bool

ruleP = (,) <$> (patternP <* string " => ") <*> patternP

patternP = parsePattern <$> many1 (noneOf "/ ") `sepBy1` char '/'

onOrOff '#' = True
onOrOff '.' = False

variants pattern = [pattern] ++ rotations pattern ++ foldMap rotations (flips pattern)

rotations pattern = fmap ($ pattern) [rotate, rotate . rotate, rotate . rotate . rotate]
rotate = tr . fromLists . reverse . toLists

flipMatrix = fromLists . reverse . toLists

flips pattern = fmap ($ pattern) [flipMatrix, tr . flipMatrix . tr]

allRules = foldMap $ (uncurry zip . bimap variants repeat) . either undefined id . parse ruleP ""

-- must use force, since the matrix library seems to have a bug regarding equality
breakIntoSquares squareSize matrix = fmap force $ [subMatrix (x-1,y-1) (x+squareSize-2,y+squareSize-2) matrix | x <- [1,(squareSize+1)..rows matrix], y <- [1,(squareSize+1)..rows matrix]]

squares matrix = breakIntoSquares (if rows matrix `mod` 2 == 0 then 2 else 3) matrix

enhance _     0         = id
enhance rules iteration = enhance rules (iteration - 1::Int) . joinMatrices . fmap (substitute rules) . squares

substitute rules pattern = snd . fromJust . find ((== pattern) . fst) $ rules

joinMatrices :: [Pattern] -> Pattern
joinMatrices matrices = fromBlocks False $ chunksOf (floor . (sqrt :: Double -> Double) . fromIntegral . length $ matrices) matrices

solve iterations = length . filter id . toList . (\inp -> enhance (allRules inp) iterations initialImage)

-- "How many pixels stay on after 5 iterations"
solution1 = solve 5 <$> input

-- "How many pixels stay on after 18 iterations"
solution2 = solve 18 <$> input
