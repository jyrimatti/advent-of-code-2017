module Day14 where

import Data.Char (intToDigit,ord)
import Data.List (cycle,intersperse,nub,sort)
import Data.List.Split (chunksOf)
import Data.Bits (xor)
import Numeric (showHex,readHex,showIntAtBase)
import Data.Bifunctor (second)
import qualified Data.Vector as V
import qualified Data.Set as S 

input :: IO [String]
input = return $ ("amgozmfv-" ++) . show <$> [0..127]

input_test :: IO [String]
input_test = return $ ("flqrgnkx-" ++) . show <$> [0..127]

sublist start len list | len <= length list = take len $ drop start $ list ++ list
sublist start len list = error $ show [show start, show len, show list] 

step :: Int -> Int -> [Int] -> [Int] -> [Int]
step _   _        []      list = list
step pos skipSize lengths list = let
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
  step ((pos + len + skipSize) `mod` length list) (skipSize + 1) (tail lengths) newList

standardSuffix = [17, 31, 73, 47, 23]

denseHash = fmap (foldr1 xor) . chunksOf 16

hash input = let
  lengths = (ord <$> input) ++ standardSuffix
  times64 = take (64 * length lengths) $ cycle lengths
  toHexWithLeadingZero i = case showHex i "" of [a] -> ['0',a]; x -> x
 in
  foldMap toHexWithLeadingZero . denseHash . step 0 0 times64

pad xs = reverse $ take 4 $ reverse $ replicate 4 '0' ++ xs

toBinary = pad . flip (showIntAtBase 2 intToDigit) "" . fst . head . readHex . (\a -> [a])

grid = fmap (foldMap toBinary . flip hash [0..255])

solve1 = length . filter (== '1') . concat . grid

toMatrix = V.fromList . fmap V.fromList . grid

adjacent size (x,y) = S.fromList $ filter (\(x,y) -> x >= 0 && y >= 0 && x < size && y < size) [(x-1,y),(x,y-1),(x+1,y),(x,y+1)]

group matrix found (x,y) | (matrix V.! y) V.! x == '0' = (found,S.empty)
group matrix found coord = foldr (\c (f,g) -> let
  fnd = S.insert c f
 in if S.member c f
    then (f,g)
    else second (S.union g) $ group matrix fnd c) (found,S.singleton coord) $ adjacent (V.length matrix) coord

solve2 input = let
  matrix = toMatrix input
  size = length input - 1
 in
  length $ (S.fromList $ filter (not . null) $ fmap (snd . group matrix S.empty) $ [(x,y) | x <- [0..size], y <- [0..size]])
  
solution1 = solve1 <$> input
solution2 = solve2 <$> input