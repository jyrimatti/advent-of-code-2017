{-# LANGUAGE FlexibleContexts #-}
module Day07 where

import Data.Bifunctor (bimap)
import Data.Tuple.Extra ((&&&))
import Data.Function (on)
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
}

progP = Prog <$> (nameP <* space) <*> weightP <*> (optional (string " -> ") *> subnamesP)

nameP = many letter

weightP = read <$> between (char '(') (char ')') (many digit)

subnamesP = many1 letter `sepBy` string ", "

prog = either undefined id . parse progP ""
  
progsByName = Map.fromList . fmap (_progName &&& id) . fmap prog
  
data Program = Program {
  _name   :: String,
  _weight :: Int,
  _subs   :: [Program]
} deriving Show

-- convert list of programs to a tree of programs
enhance byName (Prog name weight subs) = Program name weight $ fmap (enhance byName . (!) byName) subs

programs = (\byName -> fmap (enhance byName) byName) . progsByName

bottom inp = let
  progs = programs inp
  allSubs = foldMap (fmap _name . _subs) $ elems progs
 in
  progs ! (head $ keys progs \\ allSubs)

totalWeight (Program _ weight subPrograms) = weight + sum (fmap totalWeight subPrograms)

weightUnbalance (Program _ _ subPrograms) = let
  subWeights = fmap (id &&& totalWeight) subPrograms
  commonWeight = head $ tail subWeights
  equalWeight = (==) `on` snd
  unbalanced = deleteBy equalWeight commonWeight (nubBy equalWeight subWeights)
 in
  case foldMap weightUnbalance subPrograms of
    [x] -> [x] -- pass the unbalance of sub programs
    _   -> fmap (uncurry (+) . bimap _weight (snd commonWeight - )) unbalanced -- return the unbalance of this program

-- "What is the name of the bottom program?"
solve1 = _name . bottom

-- "what would its weight need to be to balance the entire tower"
solve2 = head . weightUnbalance . bottom

solution1 = solve1 <$> input
solution2 = solve2 <$> input