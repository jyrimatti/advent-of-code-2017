{-# LANGUAGE FlexibleContexts #-}
module Day16 where

import Prelude hiding (length,(++),take,drop)
import Control.Applicative ((<|>))
import Data.Maybe (fromJust)
import Data.Foldable (foldl')

import qualified Data.Vector.Unboxed as Vec (fromList)
import Data.Vector.Unboxed (length,(++),take,drop,(//),(!),elemIndex)

import Text.Parsec (parse)
import Text.Parsec.Char (char,letter)
import Text.Parsec.Combinator (sepBy1)
import Text.ParserCombinators.Parsec.Number (int)

input = readFile "input/input16.txt"

input_test :: IO String
input_test = return "s1,x3/4,pe/b"

type Index = Int
  
data Step = Spin Int
          | Exchange Index Index
          | Partner Char Char
  deriving Show

danceP = stepP `sepBy1` (char ',')

stepP = spinP <|> exchangeP <|> partnerP

spinP = Spin <$> (char 's' *> int)

exchangeP = Exchange <$> (char 'x' *> int) <*> (char '/' *> int)

partnerP = Partner <$> (char 'p' *> letter) <*> (char '/' *> letter)



dance = either undefined id . parse danceP ""

name2index name = fromJust . elemIndex name

eval program (Spin steps) = take (length program) $ drop (length program - steps) program ++ program

eval program (Exchange pos1 pos2) = program // [(pos1, program ! pos2), (pos2, program ! pos1)]

eval program (Partner name1 name2) = eval program (Exchange (name2index name1 program) (name2index name2 program))

programs = Vec.fromList ['a'..'p']

solve initialRepeat initialProgs inp = solve_ 0 initialProgs (dance inp)
  where solve_ rep progs _ | rep == initialRepeat                     = progs
        -- let's assume the program eventually repeat themselves, so we can exit early...
        solve_ rep progs _ | rep /= (0::Int) && progs == initialProgs = solve (initialRepeat `mod` rep) initialProgs inp
        solve_ rep progs moves                                        = solve_ (rep+1) (foldl' eval progs moves) moves

-- "In what order are the programs standing"
solve1 = solve 1 programs

-- "In what order are the programs standing"
solve2 = solve 1000000000 programs

solution1 = solve1 <$> input
solution2 = solve2 <$> input