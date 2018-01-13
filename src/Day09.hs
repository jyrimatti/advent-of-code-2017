{-# LANGUAGE FlexibleContexts, TupleSections #-}
module Day09 where

import Text.Parsec (parse,many,(<|>),notFollowedBy,noneOf,eof)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char,anyChar)
import Text.Parsec.Combinator (between,sepBy1)
import Data.Tuple.Extra ((&&&))

input = readFile "input/input09.txt"

group :: Int -> Parser (Int,Int)
group level = ((level +) . sum . fmap fst &&& sum . fmap snd) <$> between (char '{') (char '}') (contents level `sepBy1` char ',')

contents level = group (level + 1) <|>
                 (0,) <$> garbage <|>
                 pure (0,0) <* notFollowedBy eof

garbage = sum <$> between (char '<') (char '>') (many garbageContents)

garbageContents = pure 0 <* (char '!' *> anyChar) <|>
                  pure 1 <* noneOf ['>']

solve = either undefined id . parse (group 1) ""

solution1 = fst <$> solve <$> input
solution2 = snd <$> solve <$> input