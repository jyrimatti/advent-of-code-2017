module Day11 where

import Data.Maybe (fromJust)
import Data.Text (splitOn,pack,unpack)

import Algorithm.Search (aStar)

input = fmap unpack . splitOn (pack ",") . pack <$> readFile "input/input11.txt"

move (x,y) "n"  = (x  ,y+1)
move (x,y) "s"  = (x  ,y-1)
move (x,y) "nw" = (x-1,y)
move (x,y) "sw" = (x-1,y-1)
move (x,y) "se" = (x+1,y)
move (x,y) "ne" = (x+1,y+1)

walk start [] = [start]
walk start (x:xs) = start : walk (move start x) xs

distance :: (Int,Int) -> (Int,Int) -> Double
distance (ax,ay) (bx,by) = sqrt $ fromIntegral $ (ax-bx)^(2::Int) + (ay-by)^(2::Int)

neighbouring hex = fmap (move hex) ["n", "s", "nw", "sw", "se", "ne"]

solve goal = fromJust $ aStar neighbouring (const $ const 1) (distance goal) (== goal) (0,0)

solve1 = length . snd . solve . last . walk (0,0)

solve2 = maximum . fmap (fst . solve) . walk (0,0)

solution1 = solve1 <$> input
solution2 = solve2 <$> input