{-# LANGUAGE FlexibleContexts #-}
module Day07 where

import Prelude hiding (map)
import Text.Parsec (parse,many,many1,optional)
import Text.Parsec.Char (char,space,string,letter,digit)
import Text.Parsec.Combinator (between,sepBy)
import Data.Map.Strict (fromList,(!),elems,keys,map)
import Data.List ((\\),nubBy,deleteBy)
  
data Prog = Prog {
  _name :: String,
  _weight :: Int,
  _subnames :: [String]
} deriving Show

progP = Prog <$> (nameP <* space) <*> weightP <*> (optional (string " -> ") *> subnamesP)

nameP = many letter

weightP = read <$> between (char '(') (char ')') (many digit)

subnamesP = many1 letter `sepBy` string ", "


input = lines <$> readFile "input/input07.txt"
input_test = lines <$> readFile "input/input07_test.txt"

progs = either undefined id . parse progP ""
  
progsMap inp = fromList $ fmap (\p@(Prog n _ _) -> (n,p)) (progs <$> inp)
  
data Program = Program {
  name :: String,
  weight :: Int,
  subs :: [Program]
} deriving Show

enhance pMap (Prog n w s) = Program n w $ fmap (enhance pMap . (!) pMap) s

programs inp = let
  pMap = progsMap inp
 in
  map (enhance pMap) pMap

root inp = let
  prgs = programs inp
  allSubs = concatMap (fmap name . subs) $ elems prgs
 in
  prgs ! (head $ keys prgs\\ allSubs)
  
rootName = name . root

totalWeight (Program _ w ss) = w + sum (fmap totalWeight ss)

weightUnbalance (Program _ _ ss) = let
  subWeights = fmap (\s -> (s,totalWeight s)) ss
  commonWeight = head $ tail subWeights
  cmp a b = snd a == snd b
  unbalanced = deleteBy cmp commonWeight (nubBy cmp subWeights)
 in
  case foldMap weightUnbalance ss of
    [x] -> [x]
    [] -> (\(s,u) -> weight s + (snd commonWeight - u)) <$> unbalanced

solve1 = rootName
solve2 = head . weightUnbalance . root

solution1 = solve1 <$> input
solution2 = solve2 <$> input