{-# LANGUAGE OverloadedStrings #-}
module Day11 where

import Algorithm.Search (aStar)
import Data.Text (splitOn,pack,unpack)
import Data.HashSet (fromList)
import Data.Maybe (fromJust)
import Data.Foldable (maximumBy)
import Data.Ord (comparing)

input = fmap unpack . splitOn "," . pack <$> readFile "input/input11.txt"

move (x,y) "n" = (x,y+1)
move (x,y) "s" = (x,y-1)
move (x,y) "nw" = (x-1,y)
move (x,y) "sw" = (x-1,y-1)
move (x,y) "se" = (x+1,y)
move (x,y) "ne" = (x+1,y+1)

walk start [] = [start]
walk start (x:xs) = start : walk (move start x) xs

distance :: Floating a => (Int,Int) -> (Int,Int) -> a
distance (ax,ay) (bx,by) = sqrt $ fromIntegral $ (ax-bx)^2 + (ay-by)^2

neighbouring hex = move hex <$> ["n", "s", "nw", "sw", "se", "ne"]

solve goal = fromJust $ aStar neighbouring (\a b -> 1) (distance goal) (== goal) (0,0)

solve1 = length . snd . solve . last . walk (0,0)

solve2 = maximum . fmap (fst . solve) . walk (0,0)

solution1 = solve1 <$> input
solution2 = solve2 <$> input