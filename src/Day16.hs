{-# LANGUAGE FlexibleContexts #-}
module Day16 where

import Prelude hiding (length,(++),take,drop)
import Control.Applicative ((<|>))
import Data.Function (on)
import Data.Char (chr)
import Data.Maybe (fromJust)
import Data.Foldable (foldl')

import qualified Data.Vector.Unboxed as Vec (fromList)
import Data.Vector.Unboxed (length,(++),take,drop,(//),(!),elemIndex)

import Text.Parsec (parse,many1)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char,digit,letter)
import Text.Parsec.Combinator (sepBy1)

input = readFile "input/input16.txt"

input_test :: IO String
input_test = return "s1,x3/4,pe/b"

type Index = Int
  
data Step = Spin Int | Exchange Index Index | Partner Char Char
  deriving Show



danceP :: Parser [Step]
danceP = stepP `sepBy1` (char ',')

stepP = spinP <|> exchangeP <|> partnerP

numberP = many1 digit

spinP = Spin . read <$> (char 's' *> numberP)

exchangeP = on Exchange read <$> (char 'x' *> numberP) <*> (char '/' *> numberP)

partnerP = Partner <$> (char 'p' *> letter) <*> (char '/' *> letter)



dance = either undefined id . parse danceP ""

name2index name = fromJust . elemIndex name
index2name = chr . (+ 97)

eval (Spin x) ps = take (length ps) $ drop (length ps - x) ps ++ ps

eval (Exchange i1 i2) ps = ps // [(i1, ps ! i2),(i2, ps ! i1)]

eval (Partner c1 c2) ps = eval (Exchange (name2index c1 ps) (name2index c2 ps)) ps

programs n = Vec.fromList $ fmap index2name [0..n-1]

solve initialRepeat initialProgs inp = solve_ 0 initialProgs (dance inp)
  where solve_ rep progs _ | rep == initialRepeat                     = progs
        -- let's assume the program eventually repeat themselves, so we can exit early...
        solve_ rep progs _ | rep /= (0::Int) && progs == initialProgs = solve (initialRepeat `mod` rep) initialProgs inp
        solve_ rep progs dan                                          = solve_ (rep+1) (foldl' (flip eval) progs dan) dan

solve1 = solve 1 (programs 16)
solve2 = solve 1000000000 (programs 16)

solution1 = solve1 <$> input
solution2 = solve2 <$> input