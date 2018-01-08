{-# LANGUAGE FlexibleContexts #-}
module Day16 where

import Prelude hiding (length,(++),take)
import Control.Applicative ((<|>))
import Text.Parsec (parse,many1)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char,digit,letter)
import Text.Parsec.Combinator (sepBy1)
import Data.Vector (fromList,slice,length,(++),take,(//),(!),elemIndex)
import Data.Function (on)
import Data.Char (chr)
import Data.Maybe (fromJust)
import Data.Foldable (foldl')

input = readFile "input/input16.txt"

input_test :: IO String
input_test = return "s1,x3/4,pe/b"

type Index = Int
  
data Step = Spin Int | Exchange Index Index | Partner Char Char
  deriving Show



danceP :: Parser [Step]
danceP = stepP `sepBy1` (char ',')

stepP = spinP <|> exchangeP <|> partnerP

spinP = Spin . read <$> (char 's' *> many1 digit)

exchangeP = on Exchange read <$> (char 'x' *> many1 digit) <*> (char '/' *> many1 digit)

partnerP = Partner <$> (char 'p' *> letter) <*> (char '/' *> letter)



dance = either undefined id . parse danceP ""

name2index name = fromJust . elemIndex name
index2name = chr . (\n -> n + 97)

eval (Spin x) ps = (slice (length ps - x) x ps) ++ (take (length ps - x) ps)

eval (Exchange i1 i2) ps = let
  e1 = ps ! i1
  e2 = ps ! i2
 in
  ps // [(i1,e2),(i2,e1)]

eval (Partner c1 c2) ps = eval (Exchange (name2index c1 ps) (name2index c2 ps)) ps

programs n = fromList $ fmap index2name [0..n-1]

solve initialRepeat initialProgs inp = solve_ 0 initialProgs (dance inp)
  where solve_ rep progs _ | rep == initialRepeat = progs
        -- let's assume the program eventually repeat themselves, so we can exit early...
        solve_ rep progs _ | rep /= (0::Int) && progs == initialProgs = solve (initialRepeat `mod` rep) initialProgs inp
        solve_ rep progs dan = solve_ (rep+1) (foldl' (flip eval) progs dan) dan

solve1 = solve 1 (programs 16)
solve2 = solve 1000000000 (programs 16)

solution1 = solve1 <$> input
solution2 = solve2 <$> input