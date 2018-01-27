{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Day10 where

import Data.Char (ord)
import Data.Bits (xor)
import Numeric (showHex)
import Data.Text (splitOn,pack,unpack)
import Data.List.Split (chunksOf)

parseAsLengths = fmap read <$> fmap unpack . splitOn "," . pack

parseAsCharacters = fmap ord

input      parse = parse <$> readFile "input/input10.txt"
input_test parse = parse <$> readFile "input/input10_test.txt"

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

-- "what is the result of multiplying the first two numbers in the list"
solve1 listOfNumbers = product . take 2 . firstStep listOfNumbers

standardSuffix = [17, 31, 73, 47, 23]

-- "use numeric bitwise XOR to combine each consecutive block of 16 numbers in the sparse hash"
denseHash = fmap (foldr1 xor) . chunksOf 16

-- not the famous js-thing ;) This just ensures each hex is 2 characters
leftpad [a]   = ['0', a]
leftpad [a,b] = [a,b]

toHexWithLeadingZero i = leftpad (showHex i "")

-- "what is the Knot Hash of your puzzle input"
solve2 listOfNumbers = foldMap toHexWithLeadingZero . denseHash . firstStep listOfNumbers . concat . replicate 64 . (++ standardSuffix)

solution1 = solve1 [0..255] <$> input parseAsLengths
solution2 = solve2 [0..255] <$> input parseAsCharacters