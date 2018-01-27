{-# LANGUAGE FlexibleContexts, TupleSections #-}
module Day19 where

import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Data.List ((\\),unfoldr)
import Data.Vector.Unboxed (elemIndex)
import Data.Matrix.Unboxed (Matrix,(!),fromLists,takeRow,rows,cols)

parse :: String -> Matrix Char
parse = fromLists <$> lines

input      = parse <$> readFile "input/input19.txt"
input_test = parse <$> readFile "input/input19_test.txt"

-- start on vertical bar on the first row
startCoordinate = (,0) . fromJust . elemIndex '|' . flip takeRow 0

withinBounds width height (x,y) = x >= 0 && y >= 0 && x < width && y < height

adjacent width height (x,y) = filter (withinBounds width height) [(x-1,y),(x,y-1),(x+1,y),(x,y+1)]

(!!!) grid = (grid !) . swap

(x,y) +.+ (a,b) = (x+a,y+b)
(x,y) -.- (a,b) = (x-a,y-b)

nonemptyCoordinate grid = (/= ' ') . (grid !!!)

nextDirection _                   currentLocation grid | currentLocation == startCoordinate grid = (0,1)
nextDirection previousCoordinates currentLocation grid = case filter (nonemptyCoordinate grid) $ adjacent (cols grid) (rows grid) currentLocation of
    [_]    -> (0,0) -- Only one non-empty adjacent location -> in goal!
    xs -> let
                [c] = xs \\ previousCoordinates
              in
                c -.- currentLocation

straightLine direction = unfoldr $ \currentLocation -> Just (currentLocation, currentLocation +.+ direction)

isOneOf = flip elem

untilTurnOrEnd grid = takeWhile $ not . isOneOf ['+',' '] . (grid !!!)

letterCoordinates grid = isOneOf ['A'..'Z'] . (grid !!!)

walk grid previousCoordinates currentLocation = let
    direction = nextDirection previousCoordinates currentLocation grid
    path = untilTurnOrEnd grid $ straightLine direction currentLocation
    lettersOnPath = fmap (grid !!!) $ filter (letterCoordinates grid) path
    newLocation = last (currentLocation : path) +.+ direction
    steps = if length path == 0 then 0 else length path + 1
  in
    case direction of
      (0,0) -> ([],-1) -- don't move -> in goal!
      _     -> let (foundLetters,distanceTravelled) = walk grid (currentLocation : path) newLocation
                in (lettersOnPath ++ foundLetters, distanceTravelled + steps)

solve grid = walk grid [] (startCoordinate grid)

-- "What letters will it see"
solution1 = fst . solve <$> input

-- "How many steps does the packet need to go"
solution2 = snd . solve <$> input