module Day11 where

import Data.Maybe (fromJust)
import Data.Text (splitOn,pack,unpack)

import Algorithm.Search (aStar)

parse = fmap unpack . splitOn (pack ",") . pack

input      = parse <$> readFile "input/input11.txt"

input_test :: IO [String]
input_test = pure $ parse "se,sw,se,sw,sw"

type Coordinate = (Int,Int)

move (x,y) "n"  = (x  ,y+1)
move (x,y) "s"  = (x  ,y-1)
move (x,y) "nw" = (x-1,y)
move (x,y) "sw" = (x-1,y-1)
move (x,y) "se" = (x+1,y)
move (x,y) "ne" = (x+1,y+1)

walk start [] = [start]
walk start (x:xs) = start : walk (move start x) xs

squared :: Int -> Int
squared = (^ (2::Int))

distance :: Coordinate -> Coordinate -> Double
distance (ax,ay) (bx,by) = sqrt $ fromIntegral $ squared (ax-bx) + squared (ay-by)

neighbouring hex = fmap (move hex) ["n", "s", "nw", "sw", "se", "ne"]

numberOfStepsTo goal = fromJust $ fmap fst $ aStar neighbouring (const $ const 1) (distance goal) (== goal) (0,0)

-- "the fewest number of steps required to reach him"
solve1 = numberOfStepsTo . last . walk (0,0)

-- "How many steps away is the furthest he ever got"
solve2 = maximum . fmap numberOfStepsTo . walk (0,0)

solution1 = solve1 <$> input
solution2 = solve2 <$> input