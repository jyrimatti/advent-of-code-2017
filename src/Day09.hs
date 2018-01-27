{-# LANGUAGE FlexibleContexts, TupleSections #-}
module Day09 where

import Data.Tuple.Extra (both)
import Data.Bifunctor (first)
import Data.List (unzip)

import Text.Parsec (parse,many,(<|>),noneOf)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char,anyChar)
import Text.Parsec.Combinator (between,sepBy)

input = readFile "input/input09.txt"

type TotalScoreAndNonCancelled = (Int,Int)

groupP :: Int -> Parser TotalScoreAndNonCancelled
groupP level = first (level +) . both sum . unzip <$> between (char '{') (char '}') (contentsP level `sepBy` char ',')

contentsP level = groupP (level + 1) <|>
                  (0,) <$> garbageP -- garbage always has 0 score

garbageP = sum <$> between (char '<') (char '>') (many garbageContentsP)

garbageContentsP = pure 0 <* (char '!' *> anyChar) <|> -- cancelled char
                   pure 1 <* noneOf ['>']              -- non-cancelled char

solve = either undefined id . parse (groupP 1) ""

solution1 = fst <$> solve <$> input
solution2 = snd <$> solve <$> input