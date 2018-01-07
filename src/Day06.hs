module Day06 where

import Prelude hiding (length,elem,iterate)
import qualified Prelude as P
import Data.Vector (Vector,fromList,maxIndex,length,(!),(//))
import Data.List (elemIndex,find)
import Data.Maybe (isJust,fromJust)
import Data.Bifunctor (bimap)

input = fmap read <$> fromList . words <$> readFile "input/input06.txt"

step banks = let
  indexToDistribute = maxIndex banks
  amountOfBanks = length banks
  blocksToDistribute = banks ! indexToDistribute
 in
  banks // [(indexToDistribute,0)]
        /// updates1 amountOfBanks blocksToDistribute indexToDistribute
        /// updates2 amountOfBanks blocksToDistribute

(///) :: Vector a -> [(Int,a -> a)] -> Vector a
(///) vect ifs = vect // fmap (\(i,f) -> (i, f (vect ! i))) ifs

updates1 amountOfBanks blocks indexOfMost = let
  updates banks startIndex bs = (\i -> (i `mod` banks, (+1))) <$> take bs [startIndex..]
 in
  updates amountOfBanks (indexOfMost+1) (blocks `rem` amountOfBanks)

updates2 amountOfBanks blocks = (\i -> (i, (+ blocks `div` amountOfBanks))) <$> [0..amountOfBanks-1]

iterate f banks = let res = f banks in res : iterate f res
steps = iterate step

acc (seen,_) res = (res : seen, (+1) <$> elemIndex res seen)

solve = bimap P.length id . fromJust . find (isJust.snd) . scanl acc ([],Nothing) . steps

solve1 = fst . solve
solve2 = fromJust . snd . solve

solution1 = solve1 <$> input
solution2 = solve2 <$> input