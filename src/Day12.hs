{-# LANGUAGE FlexibleContexts #-}
module Day12 where

import Data.Tuple.Extra ((&&&))

import qualified Data.Map.Strict as Map (fromList)
import Data.Map.Strict ((!),elems)
import qualified Data.HashSet as Set (fromList)
import Data.HashSet (empty,member,insert)

import Text.Parsec (parse,many1)
import Text.Parsec.Char (string, noneOf)
import Text.Parsec.Combinator (sepBy1)
  
input      = lines <$> readFile "input/input12.txt"
input_test = lines <$> readFile "input/input12_test.txt"

data Prog = Prog {
  _progName       :: String,
  _connectedProgs :: [String]
}

progP = Prog <$> nameP <*> (string " <-> " *> nameP `sepBy1` (string ", "))

nameP = many1 $ noneOf ", "

data Program = Program {
  _name      :: String,
  _connected :: [Program]
} deriving Show

prog = either undefined id . parse progP ""

progsByName = Map.fromList . fmap (_progName &&& id) . fmap prog

enhance byName (Prog name connected) = Program name $ fmap (enhance byName . (!) byName) connected

programs = (\byName -> fmap (enhance byName) byName) . progsByName

reachable seen (Program name _) | member name seen = seen
reachable seen (Program name connected)            = foldMap (reachable (insert name seen)) connected

-- reachable programs from the given one
reachableFrom = reachable empty

-- "How many programs are in the group that contains program ID 0"
solve1 = length . reachableFrom . (! "0") . programs

-- "How many groups are there in total"
solve2 = length . Set.fromList . fmap reachableFrom . elems . programs

solution1 = solve1 <$> input
solution2 = solve2 <$> input
