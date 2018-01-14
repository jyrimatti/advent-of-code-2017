{-# LANGUAGE FlexibleContexts #-}
module Day14 where

import Data.Char (intToDigit,ord)
import Data.Bits (xor)
import Data.Text (takeEnd,pack,unpack)
import Data.Bifunctor (second)
import Numeric (showHex,readHex,showIntAtBase)

import Data.List.Split (chunksOf)
import Data.Matrix.Unboxed (Matrix,(!),fromLists,rows)
import qualified Data.Set as Set (fromList,empty)
import Data.Set (insert,member,union,singleton)

input = ("amgozmfv-" ++) . show <$> [0..127::Int]

input_test = ("flqrgnkx-" ++) . show <$> [0..127::Int]

sublist start len = take len . drop start . cycle

step :: Int -> Int -> [Int] -> [Int] -> [Int]
step _   _        list []      = list
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

standardSuffix = [17, 31, 73, 47, 23]

denseHash = fmap (foldr1 xor) . chunksOf 16

leftpad [a] = ['0', a]
leftpad a   = a

hash list inp = let
  lengths = fmap ord inp ++ standardSuffix
  times64 = take (64 * length lengths) $ cycle lengths
  toHexWithLeadingZero i = leftpad (showHex i "")
 in
  foldMap toHexWithLeadingZero . denseHash . step 0 0 list $ times64

pad = unpack . takeEnd 4 . pack . (replicate 4 '0' ++)

hex2int :: Char -> Int
hex2int = fst . head . readHex . (\a -> [a])

toBinary = pad . flip (showIntAtBase 2 intToDigit) "" . hex2int

boolP '1' = True
boolP '0' = False

grid = fmap (fmap boolP) . fmap (foldMap toBinary . hash [0..255])

solve1 = length . filter (== True) . concat . grid

toMatrix :: [String] -> Matrix Bool
toMatrix = fromLists . grid

withinBounds size (x,y) = x >= 0 && y >= 0 && x < size && y < size

adjacent size (x,y) = Set.fromList $ filter (withinBounds size) [(x-1,y),(x,y-1),(x+1,y),(x,y+1)]

group matrix found coord | matrix ! coord == False = (found,Set.empty)
group matrix found coord                           = foldr (\c (f,g) -> if member c f
    then (f,g)
    else second (union g) $ group matrix (insert c f) c) (found,singleton coord) $ adjacent (rows matrix) coord

solve2 inp = let
  matrix = toMatrix inp
  size = length inp - 1
 in
  length . Set.fromList . filter (not . null) . fmap (snd . group matrix Set.empty) $ [(x,y) | x <- [0..size], y <- [0..size]]
  
solution1 = solve1 input
solution2 = solve2 input