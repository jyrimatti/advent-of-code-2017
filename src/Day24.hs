{-# LANGUAGE OverloadedStrings #-}
module Day24 where

import Prelude hiding (readFile,lines)
import Data.Text (Text,splitOn,lines,unpack)
import Data.Text.IO (readFile)
import Data.List (delete,sortBy,groupBy,maximumBy)
import Data.Ord (comparing)

input = readFile "input/input24.txt"
input_test = readFile "input/input24_test.txt"

parse :: Text -> [(Int,Int)]
parse = fmap ((\[a,b] -> (read (unpack a),read (unpack b))) . splitOn "/") . lines

strength (a,b) = a+b

fits ((c,d):_) (a,b) = a == c || b == c

takePiece bridge@((a,_):_) ports = do
    fitting@(c,d) <- filter (fits bridge) ports
    let fitted = if a == d then (c,d) else (d,c)
    (fitted:bridge) : (takePiece (fitted:bridge) $ delete fitting ports)

solve = fmap (fmap (\(a,b) -> (b,a))) . fmap reverse . fmap init . takePiece [(0,0)] . parse

solve1 = maximum . fmap (sum . fmap strength) . solve

solve2 = snd . maximumBy (comparing snd) . head . groupBy (\a b -> fst a == fst b) . reverse . sortBy (comparing fst) . fmap (\b -> (length b, sum $ fmap strength b)) . solve

solution1 = solve1 <$> input
solution2 = solve2 <$> input