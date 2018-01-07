{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Day10 where

import Data.Char (chr,ord)
import Data.List (cycle,intersperse)
import Data.List.Split (chunksOf)
import Data.Bits (xor)
import Numeric (showHex)
import Data.Text (splitOn,pack,unpack)

parse1 = fmap read <$> fmap unpack . splitOn "," . pack
parse2 = id

input parse = parse <$> readFile "input/input10.txt"

input_test parse = parse <$> readFile "input/input10_test.txt"

sublist start len list | len <= length list = take len $ drop start $ list ++ list
sublist start len list = error $ show [show start, show len, show list] 

step :: Int -> Int -> [Int] -> [Int] -> [Int]
step _   _        list [] = list
step pos skipSize list lengths = let
  len = head lengths
  section = sublist pos len list
  reversed = reverse section
  prefix = take pos list
  suffix = drop (pos + len) list
  tempList = prefix ++ reversed ++ suffix
  overflowing = drop (length list) tempList
  remaining = drop (length overflowing) tempList
  newList = take (length list) $ overflowing ++ remaining
 in
  step ((pos + len + skipSize) `mod` length list) (skipSize + 1) newList (tail lengths)
  
solve1 list = product . take 2 . step 0 0 list

standardSuffix = [17, 31, 73, 47, 23]

denseHash = fmap (foldr1 xor) . chunksOf 16

solve2 list input = let
  lengths = (ord <$> input) ++ standardSuffix
  times64 = take (64 * length lengths) $ cycle lengths
  toHexWithLeadingZero i = case showHex i "" of [a] -> ['0',a]; x -> x
 in
  foldMap toHexWithLeadingZero . denseHash . step 0 0 list $ times64
  
solution1 = solve1 [0..255] <$> input parse1
solution2 = solve2 [0..255] <$> input parse2