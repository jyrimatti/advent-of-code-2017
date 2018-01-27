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

input      = ("amgozmfv-" ++) . show <$> [0..127::Int]
input_test = ("flqrgnkx-" ++) . show <$> [0..127::Int]

-- cyclic sublist with offset and length
sublist offset len = take len . drop offset . cycle

step _   _        listOfNumbers []      = listOfNumbers
step currentPosition skipSize listOfNumbers (first:rest) = let
  section = sublist currentPosition first listOfNumbers
  prefix = take currentPosition listOfNumbers
  suffix = drop (currentPosition + first) listOfNumbers
  tempList = prefix ++ reverse section ++ suffix
  overflowing = drop (length listOfNumbers) tempList
  remaining = drop (length overflowing) tempList
  newList = take (length listOfNumbers) $ overflowing ++ remaining
 in
  step ((currentPosition + first + skipSize) `mod` length listOfNumbers) (skipSize + 1) newList rest

firstStep :: [Int] -> [Int] -> [Int]
firstStep = step 0 0

standardSuffix = [17, 31, 73, 47, 23]

-- "use numeric bitwise XOR to combine each consecutive block of 16 numbers in the sparse hash"
denseHash = fmap (foldr1 xor) . chunksOf 16

-- not the famous js-thing ;) This just ensures each hex is 2 characters
leftpad [a]   = ['0', a]
leftpad [a,b] = [a,b]

toHexWithLeadingZero i = leftpad (showHex i "")

hash listOfNumbers = foldMap toHexWithLeadingZero . denseHash . firstStep listOfNumbers . concat . replicate 64 . (++ standardSuffix) . fmap ord

pad = unpack . takeEnd 4 . pack . (replicate 4 '0' ++)

hex2int :: Char -> Int
hex2int = fst . head . readHex . (\a -> [a])

toBinary = pad . flip (showIntAtBase 2 intToDigit) "" . hex2int

boolP '1' = True
boolP '0' = False

grid = fmap (fmap boolP . foldMap toBinary . hash [0..255])

-- "how many squares are used"
solve1 = length . filter id . concat . grid

toMatrix :: [String] -> Matrix Bool
toMatrix = fromLists . grid

withinBounds size (x,y) = x >= 0 && y >= 0 && x < size && y < size

adjacent size (x,y) = Set.fromList $ filter (withinBounds size) [(x-1,y),(x,y-1),(x+1,y),(x,y+1)]


acc _      coordinate (seen, group) | member coordinate seen = (seen,group)
acc matrix coordinate (seen, group)                          = second (union group) $ findGroup matrix (insert coordinate seen) coordinate

findGroup matrix seen coord | matrix ! coord == False = (seen,Set.empty)
findGroup matrix seen coord                           = foldr (acc matrix) (seen,singleton coord) $ adjacent (rows matrix) coord

-- "How many regions are present"
solve2 inp = let
  size = length inp - 1
  groupFinder = snd . findGroup (toMatrix inp) Set.empty
 in
  length . Set.fromList . filter (not . null) . fmap groupFinder $ [(x,y) | x <- [0..size], y <- [0..size]]
  
solution1 = solve1 input
solution2 = solve2 input