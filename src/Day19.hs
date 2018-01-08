module Day19 where

import Prelude hiding ()
import Data.Vector (fromList,(!),elemIndex)
import Data.Maybe (fromJust)
import Data.List ((\\),unfoldr)

type Coord = (Int,Int)

parse = fmap fromList <$> fromList <$> lines

input = parse <$> readFile "input/input19.txt"
input_test = parse <$> readFile "input/input19_test.txt"

start inp = (fromJust $ elemIndex '|' $ inp ! 0, 0)

adjacent width height (x,y) = filter (\(aa,bb) -> aa >= 0 && bb >= 0 && aa < width && bb < height) [(x-1,y),(x,y-1),(x+1,y),(x,y+1)]

get inp (x,y) = inp ! y ! x

(x,y) .+ (a,b) = (x+a,y+b)
(x,y) .- (a,b) = (x-a,y-b)

nonempty inp = filter $ (/= ' ') . get inp

direction _     loc inp | loc == start inp = (0,1)
direction prevs loc inp                    = case nonempty inp $ adjacent (length $ inp ! 0) (length inp) loc of
    [_]    -> (0,0) -- in goal!
    double -> let [c] = double \\ prevs in c .- loc

line diff = unfoldr $ \loc -> Just (loc, loc .+ diff)

untilTurnOrEnd inp = takeWhile $ \c -> not $ get inp c `elem` "+ "

filterLetters inp = filter $ \c -> get inp c `elem` ['A'..'Z']

walk inp prevs loc = let
    dir = direction prevs loc inp
    steps = untilTurnOrEnd inp $ line dir loc
    letters = get inp <$> (filterLetters inp) steps
    final = last (loc : steps) .+ dir
    count = if length steps == 0 then 0 else length steps + 1
  in
    if dir == (0,0)
      then ([],-1)
      else let (lx,c) = walk inp (loc : steps) final
            in (letters ++ lx, count + c)

solve inp = walk inp [] (start inp)

solution1 = fst . solve <$> input
solution2 = snd . solve <$> input