{-# LANGUAGE FlexibleContexts #-}
module Day07 where

import Text.Parsec (parse,many,many1,optional)
import Text.Parsec.Char (char,space,string,letter,digit)
import Text.Parsec.Combinator (between,sepBy)
import qualified Data.Map.Strict as Map (fromList)
import Data.Map.Strict ((!),elems,keys)
import Data.List ((\\),nubBy,deleteBy)
import Data.Tuple.Extra ((&&&))
import Data.Bifunctor (bimap)
  
input      = lines <$> readFile "input/input07.txt"
input_test = lines <$> readFile "input/input07_test.txt"

data Prog = Prog {
  _name     :: String,
  _weight   :: Int,
  _subnames :: [String]
} deriving Show

progP = Prog <$> (nameP <* space) <*> weightP <*> (optional (string " -> ") *> subnamesP)

nameP = many letter

weightP = read <$> between (char '(') (char ')') (many digit)

subnamesP = many1 letter `sepBy` string ", "

prog = either undefined id . parse progP ""
  
progsMap = Map.fromList . fmap (_name &&& id) . fmap prog
  
data Program = Program {
  name   :: String,
  weight :: Int,
  subs   :: [Program]
} deriving Show

enhance pMap (Prog n w s) = Program n w $ fmap (enhance pMap . (!) pMap) s

programs inp = let
  pMap = progsMap inp
 in
  fmap (enhance pMap) pMap

root inp = let
  prgs = programs inp
  allSubs = foldMap (fmap name . subs) $ elems prgs
 in
  prgs ! (head $ keys prgs \\ allSubs)

totalWeight (Program _ w ss) = w + sum (fmap totalWeight ss)

weightUnbalance (Program _ _ ss) = let
  subWeights = fmap (id &&& totalWeight) ss
  commonWeight = head $ tail subWeights
  cmp a b = snd a == snd b
  unbalanced = deleteBy cmp commonWeight (nubBy cmp subWeights)
 in
  case foldMap weightUnbalance ss of
    [x] -> [x]
    [] -> fmap (uncurry (+) . bimap weight (snd commonWeight - )) unbalanced

solve1 = name . root
solve2 = head . weightUnbalance . root

solution1 = solve1 <$> input
solution2 = solve2 <$> input