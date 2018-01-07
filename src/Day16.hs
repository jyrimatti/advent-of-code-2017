{-# LANGUAGE FlexibleContexts #-}
module Day16 where

import qualified Prelude as P
import Prelude hiding (length,(++),take)
import Control.Applicative ((<*>),(<|>))
import Text.Parsec (parse,many1)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char,digit,letter)
import Text.Parsec.Combinator (sepBy1,many1)
import Data.Vector (fromList,slice,length,(++),take,(//),(!),elemIndex)
import Data.Function (on)
import Data.Char (ord,chr)
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



dance input = either undefined id $ parse danceP "" $ input

name2index name programs = fromJust $ elemIndex name programs
index2name = chr . (\n -> n + 97)

eval (Spin x) programs = (slice (length programs - x) x programs) ++ (take (length programs - x) programs)

eval (Exchange i1 i2) programs = let
  e1 = programs ! i1
  e2 = programs ! i2
 in
  programs // [(i1,e2),(i2,e1)]

eval (Partner c1 c2) programs = eval (Exchange (name2index c1 programs) (name2index c2 programs)) programs

programs n = fromList $ fmap index2name [0..n-1]

solve initialRepeat initialProgs input = solve_ 0 initialProgs (dance input)
  where solve_ repeat progs _ | repeat == initialRepeat = progs
        -- let's assume the program eventually repeat themselves, so we can exit early...
        solve_ repeat progs _ | repeat /= 0 && progs == initialProgs = solve (initialRepeat `mod` repeat) initialProgs input
        solve_ repeat progs dance = solve_ (repeat+1) (foldl' (flip eval) progs dance) dance

solve1 = solve 1 (programs 16)
solve2 = solve 1000000000 (programs 16)

solution1 = solve1 <$> input
solution2 = solve2 <$> input