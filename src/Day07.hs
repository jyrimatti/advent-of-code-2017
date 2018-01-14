{-# LANGUAGE FlexibleContexts #-}
module Day07 where

import Data.Bifunctor (bimap)
import Data.Tuple.Extra ((&&&))
import Data.List ((\\),nubBy,deleteBy)
import qualified Data.Map.Strict as Map (fromList)
import Data.Map.Strict ((!),elems,keys)

import Text.Parsec (parse,many,many1,optional)
import Text.Parsec.Char (char,space,string,letter,digit)
import Text.Parsec.Combinator (between,sepBy)
  
input      = lines <$> readFile "input/input07.txt"
input_test = lines <$> readFile "input/input07_test.txt"

data Prog = Prog {
  _progName     :: String,
  _progWeight   :: Int,
  _progSubnames :: [String]
} deriving Show

progP = Prog <$> (nameP <* space) <*> weightP <*> (optional (string " -> ") *> subnamesP)

nameP = many letter

weightP = read <$> between (char '(') (char ')') (many digit)

subnamesP = many1 letter `sepBy` string ", "

prog = either undefined id . parse progP ""
  
progsMap = Map.fromList . fmap (_progName &&& id) . fmap prog
  
data Program = Program {
  _name   :: String,
  _weight :: Int,
  _subs   :: [Program]
} deriving Show

enhance pMap (Prog n w s) = Program n w $ fmap (enhance pMap . (!) pMap) s

programs inp = let
  pMap = progsMap inp
 in
  fmap (enhance pMap) pMap

root inp = let
  prgs = programs inp
  allSubs = foldMap (fmap _name . _subs) $ elems prgs
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
    [] -> fmap (uncurry (+) . bimap _weight (snd commonWeight - )) unbalanced

solve1 = _name . root
solve2 = head . weightUnbalance . root

solution1 = solve1 <$> input
solution2 = solve2 <$> input