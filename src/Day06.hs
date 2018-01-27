module Day06 where

import Data.Maybe (isJust,fromJust)
import Data.Tuple.Extra ((&&&))
import Data.List (elemIndex)
import qualified Data.Vector.Unboxed as Vec (fromList,length)
import Data.Vector.Unboxed (maxIndex,(!),(//))

-- memory banks
input = fmap read <$> words <$> readFile "input/input06.txt"

step banks = let
  indexToDistribute = maxIndex banks -- index of greatest value
  amountOfBanks = Vec.length banks
  blocksToDistribute = banks ! indexToDistribute
 in
  banks // [(indexToDistribute,0)]
        /// distributeRemaining amountOfBanks blocksToDistribute indexToDistribute
        /// distributeEqualAmount amountOfBanks blocksToDistribute

-- update value at given index
(///) vect = (vect //) . fmap (\(i,f) -> (i, f (vect ! i)))

-- distribute equal amount to every bank
distributeEqualAmount amountOfBanks blocks = let
  blocksPerBank = blocks `div` amountOfBanks
 in
  fmap (id &&& const (+ blocksPerBank)) [0..amountOfBanks-1]

-- distrubute whatever is left
distributeRemaining amountOfBanks amountOfBlocks indexToDistribute = let
  blocksNotEvenlyDistributed = amountOfBlocks `rem` amountOfBanks
  targetBanks = fmap (`mod` amountOfBanks) [indexToDistribute+1..]
 in
  fmap (id &&& const (+ 1)) $ take blocksNotEvenlyDistributed targetBanks

-- (<all encountered banks>, <index of previous encounter of this bank if any>)
seenAndIndex (seenBanks,_) banks = (banks : seenBanks, elemIndex banks seenBanks)

solve = head . filter (isJust . snd) . scanl seenAndIndex ([],Nothing) . tail . iterate step . Vec.fromList

-- "how many redistribution cycles must be completed before a configuration is produced that has been seen before"
solve1 = length . fst . solve

-- "How many cycles are in the infinite loop"
solve2 = (+1) . fromJust . snd . solve

solution1 = solve1 <$> input
solution2 = solve2 <$> input