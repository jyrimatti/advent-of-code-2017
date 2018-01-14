module Day21 where

import Data.Bifunctor (bimap)
import Data.Maybe (fromJust)
import Data.List (find)
import Data.List.Split (chunksOf)
import Data.Matrix.Unboxed (Matrix,fromLists,force,tr,toList,toLists,rows,subMatrix,fromBlocks)

import Text.Parsec (parse,many1)
import Text.Parsec.Char (string,noneOf,char)
import Text.Parsec.Combinator (sepBy1)

initial = fromLists $ fmap (fmap toBool) $ [".#.","..#","###"]

input = lines <$> readFile "input/input21.txt"

type Pattern = Matrix Bool

ruleP = (,) <$> (patternP <* string " => ") <*> patternP

patternP = force . fromLists . fmap (fmap toBool) <$> many1 (noneOf "/ ") `sepBy1` char '/'

toBool '#' = True
toBool '.' = False

variants p = [p] ++ rotations p ++ foldMap rotations (flips p)

rotations p = [rotate p, rotate $ rotate p, rotate $ rotate $ rotate p]
rotate = tr . fromLists . reverse . toLists

flips p = [flipMatrix p, tr $ flipMatrix $ tr p]
flipMatrix = fromLists . reverse . toLists

allRules = foldMap $ (uncurry zip . bimap variants repeat) . either undefined id . parse ruleP ""

parts n dim = [subMatrix (x-1,y-1) (x+n-2,y+n-2) | x <- [1,(n+1)..dim], y <- [1,(n+1)..dim]]

subs p = fmap force $ fmap ($ p) $ parts (if rows p `mod` 2 == 0 then 2 else 3) (rows p)

enhance _     0         = id
enhance rules iteration = enhance rules (iteration - 1::Int) . join . fmap (replace rules) . subs

replace rules x = snd . fromJust . find ((== x) . fst) $ rules

join :: [Pattern] -> Pattern
join ms = fromBlocks False $ chunksOf (floor . (sqrt :: Double -> Double) . fromIntegral . length $ ms) ms

solve iterations = length . filter (== True) . toList . (\inp -> enhance (allRules inp) iterations initial)

solution1 = solve 5 <$> input
solution2 = solve 18 <$> input
