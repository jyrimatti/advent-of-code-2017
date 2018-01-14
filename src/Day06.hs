module Day06 where

import Data.Maybe (isJust,fromJust)
import Data.Bifunctor (first)
import Data.List (elemIndex,find)
import qualified Data.Vector.Unboxed as Vec (fromList,length)
import Data.Vector.Unboxed (maxIndex,(!),(//))

input = fmap read <$> words <$> readFile "input/input06.txt"

step banks = let
  indexToDistribute = maxIndex banks
  amountOfBanks = Vec.length banks
  blocksToDistribute = banks ! indexToDistribute
 in
  banks // [(indexToDistribute,0)]
        /// updates1 amountOfBanks blocksToDistribute indexToDistribute
        /// updates2 amountOfBanks blocksToDistribute

(///) vect = (vect //) . fmap (\(i,f) -> (i, f (vect ! i)))

updates1 amountOfBanks blocks indexOfMost = let
  updates banks startIndex bs = (\i -> (i `mod` banks, (+1))) <$> take bs [startIndex..]
 in
  updates amountOfBanks (indexOfMost+1) (blocks `rem` amountOfBanks)

updates2 amountOfBanks blocks = (\i -> (i, (+ blocks `div` amountOfBanks))) <$> [0..amountOfBanks-1]

acc (seen,_) res = (res : seen, (+1) <$> elemIndex res seen)

solve = first length . fromJust . find (isJust . snd) . scanl acc ([],Nothing) . tail . iterate step . Vec.fromList

solve1 = fst . solve
solve2 = fromJust . snd . solve

solution1 = solve1 <$> input
solution2 = solve2 <$> input