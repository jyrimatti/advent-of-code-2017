module Day19 where

import Prelude hiding ()
import Data.Vector (fromList,(!),elemIndex)
import Data.Maybe (fromJust)
import Data.List ((\\),unfoldr,delete)

type Coord = (Int,Int)

parse = fmap fromList <$> fromList <$> lines

input = parse <$> readFile "input/input19.txt"
input_test = parse <$> readFile "input/input19_test.txt"

start input = (fromJust $ elemIndex '|' $ input ! 0, 0)

adjacent width height (x,y) = filter (\(aa,bb) -> aa >= 0 && bb >= 0 && aa < width && bb < height) [(x-1,y),(x,y-1),(x+1,y),(x,y+1)]

get input (x,y) = input ! y ! x

(x,y) .+ (a,b) = (x+a,y+b)
(x,y) .- (a,b) = (x-a,y-b)

nonempty input = filter $ (/= ' ') . get input

direction prevs loc input | loc == start input = (0,1)
direction prevs loc input = case nonempty input $ adjacent (length $ input ! 0) (length input) loc of
    [single] -> (0,0) -- in goal!
    double   -> let [c] = double \\ prevs in c .- loc

line diff = unfoldr $ \loc -> Just (loc, loc .+ diff)

untilTurnOrEnd input = takeWhile $ \c -> not $ get input c `elem` "+ "

filterLetters input = filter $ \c -> get input c `elem` ['A'..'Z']

walk input prevs loc = let
    dir = direction prevs loc input
    steps = untilTurnOrEnd input $ line dir loc
    letters = get input <$> (filterLetters input) steps
    final = last (loc : steps) .+ dir
    count = if length steps == 0 then 0 else length steps + 1
  in
    if dir == (0,0)
      then ([],-1)
      else let (lx,c) = walk input (loc : steps) final
            in (letters ++ lx, count + c)

solve input = walk input [] (start input)

solution1 = fst . solve <$> input
solution2 = snd . solve <$> input