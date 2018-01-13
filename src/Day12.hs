{-# LANGUAGE FlexibleContexts #-}
module Day12 where

import Text.Parsec (parse,many1)
import Text.Parsec.Char (string, noneOf)
import Text.Parsec.Combinator (sepBy1)
import qualified Data.Map.Strict as Map (fromList)
import Data.Map.Strict ((!),elems)
import Data.HashSet (empty,member,insert)
import Data.List (nub)
import Data.Tuple.Extra ((&&&))
  
input      = lines <$> readFile "input/input12.txt"
input_test = lines <$> readFile "input/input12_test.txt"

data Prog = Prog {
  _name      :: String,
  _neighbors :: [String]
} deriving Show

progP = Prog <$> nameP <*> (string " <-> " *> nameP `sepBy1` (string ", "))

nameP = many1 $ noneOf ", "

data Program = Program {
  name      :: String,
  neighbors :: [Program]
} deriving Show

prog = either undefined id . parse progP ""

progsMap = Map.fromList . fmap (_name &&& id) . fmap prog

enhance pMap (Prog n ns) = Program n $ fmap (enhance pMap . (pMap !)) ns

programs inp = let
  pMap = progsMap inp
 in
  fmap (enhance pMap) pMap

reachable seen (Program n _) | member n seen = seen
reachable seen (Program n ns)                = foldMap (reachable (insert n seen)) ns

solve1 = length . reachable empty . (! "0") . programs

solve2 = length . nub . fmap (reachable empty) . elems . programs

solution1 = solve1 <$> input
solution2 = solve2 <$> input
