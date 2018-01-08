{-# LANGUAGE FlexibleContexts #-}
module Day12 where

import Prelude hiding (map)
import Text.Parsec (parse,many1)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (string, noneOf)
import Text.Parsec.Combinator (sepBy1)
import Data.Map.Strict (Map,fromList,(!),elems,map)
import Data.Set (empty,member,insert)
import Data.List (nub)
  
input = lines <$> readFile "input/input12.txt"
input_test = lines <$> readFile "input/input12_test.txt"

data Prog = Prog {
  _name :: String,
  _neighbors :: [String]
} deriving Show

progP :: Parser Prog
progP = Prog <$> nameP <*> (string " <-> " *> nameP `sepBy1` (string ", "))
nameP = many1 $ noneOf ", "

data Program = Program {
  name :: String,
  neighbors :: [Program]
} deriving Show

progs = either undefined id . parse progP ""

progsMap :: [String] -> Map String Prog
progsMap inp = fromList $ fmap (\p@(Prog n ns) -> (n,p)) (progs <$> inp)

enhance pMap (Prog n ns) = Program n (fmap (\name -> enhance pMap $ pMap ! name) ns)

programs input = let
  pMap = progsMap input
 in
  map (enhance pMap) pMap

reachable seen (Program n _) | member n seen = seen
reachable seen (Program n ns) = foldMap (reachable (insert n seen)) ns

solve1 input = length $ reachable empty $ programs input ! "0"

solve2 input = length $ nub $ reachable empty <$> elems (programs input)

solution1 = solve1 <$> input
solution2 = solve2 <$> input
