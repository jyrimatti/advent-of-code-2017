module Day21 where

import Prelude hiding (flip)
import Text.Parsec (parse,many1)
import Text.Parsec.Char (string,noneOf,char)
import Text.Parsec.Combinator (sepBy1)
import Data.Matrix.Unboxed (Matrix,fromLists,force,tr,toLists,rows,subMatrix,fromBlocks)
import Data.List (find)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)

initial = fromLists $ fmap (fmap toBool) $ [".#.","..#","###"] -- .#./..#/###

input = lines <$> readFile "input/input21.txt"

type Pattern = Matrix Bool

ruleP = (,) <$> (patternP <* string " => ") <*> patternP

patternP = force . fromLists . fmap (fmap toBool) <$> many1 (noneOf "/ ") `sepBy1` char '/'

toBool '#' = True
toBool '.' = False

variants p = [p] ++ rotations p ++ foldMap rotations (flips p)

rotations p = [rotate p, rotate $ rotate p, rotate $ rotate $ rotate p]
rotate = tr . fromLists . reverse . toLists

flips p = [flip p, tr $ flip $ tr p]
flip = fromLists . reverse . toLists

allRules = foldMap $ (\(a,b) -> zip (variants a) (repeat b)) . either undefined id . parse ruleP ""

size = rows

parts n dim = [subMatrix (x-1,y-1) (x+n-2,y+n-2) | x <- [1,(n+1)..dim], y <- [1,(n+1)..dim]]

subs p = fmap force $ fmap ($ p) $ parts (if size p `mod` 2 == 0 then 2 else 3) (size p)

enhance 0 _   = id
enhance iteration rules = enhance (iteration - 1::Int) rules . join . fmap (replace rules) . subs

replace rules x = snd $ fromJust $ find ((== x) . fst) rules

join :: [Pattern] -> Pattern
join ms = fromBlocks False $ chunksOf (floor $ (sqrt :: Double -> Double) $ fromIntegral $ length ms) ms

solve iterations = do
    i <- input
    let rules = allRules i
    let res = enhance iterations rules initial
    print $ length $ filter (== True) $ concat $ toLists $ res

solution1 = solve 5
solution2 = solve 18
