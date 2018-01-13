{-# LANGUAGE FlexibleContexts, TupleSections #-}
module Day19 where

import Prelude hiding ((!!))
import Data.Vector.Unboxed (elemIndex)
import Data.Matrix.Unboxed (Matrix,(!),fromLists,takeRow,rows,cols)
import Data.Maybe (fromJust)
import Data.List ((\\),unfoldr)
import Data.Tuple (swap)

type Coord = (Int,Int)

parse :: String -> Matrix Char
parse = fromLists <$> lines

input      = parse <$> readFile "input/input19.txt"
input_test = parse <$> readFile "input/input19_test.txt"

start = (,0) . fromJust . elemIndex '|' . flip takeRow 0

withinBounds width height (x,y) = x >= 0 && y >= 0 && x < width && y < height

adjacent width height (x,y) = filter (withinBounds width height) [(x-1,y),(x,y-1),(x+1,y),(x,y+1)]

(!!) inp = (inp !) . swap

(x,y) +.+ (a,b) = (x+a,y+b)
(x,y) -.- (a,b) = (x-a,y-b)

nonempty inp = filter $ (/= ' ') . (inp !!)

direction _     loc inp | loc == start inp = (0,1)
direction prevs loc inp                    = case nonempty inp $ adjacent (cols inp) (rows inp) loc of
    [_]    -> (0,0) -- in goal!
    double -> let
                [c] = double \\ prevs
              in
                c -.- loc

line diff = unfoldr $ \loc -> Just (loc, loc +.+ diff)

isOneOf = flip elem

untilTurnOrEnd inp = takeWhile $ not . isOneOf ['+',' '] . (inp !!)

filterLetters inp = filter $ isOneOf ['A'..'Z'] . (inp !!)

walk inp prevs loc = let
    dir = direction prevs loc inp
    steps = untilTurnOrEnd inp $ line dir loc
    letters = fmap (inp !!) $ (filterLetters inp) steps
    final = last (loc : steps) +.+ dir
    count = if length steps == 0 then 0 else length steps + 1
  in
    case dir of
      (0,0) -> ([],-1)
      _     -> let (lx,c) = walk inp (loc : steps) final
                in (letters ++ lx, count + c)

solve inp = walk inp [] (start inp)

solution1 = fst . solve <$> input
solution2 = snd . solve <$> input