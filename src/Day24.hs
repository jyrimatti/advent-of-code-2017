{-# LANGUAGE OverloadedStrings #-}
module Day24 where

import Prelude hiding (readFile,lines)
import Data.Text (Text,splitOn,lines,unpack)
import Data.Text.IO (readFile)
import Data.Tuple (swap)
import Data.Tuple.Extra ((&&&),both)
import Data.Function (on)
import Data.Ord (comparing)
import Data.List (delete,sortBy,groupBy)

input      = readFile "input/input24.txt"
input_test = readFile "input/input24_test.txt"

toTuple [a,b] = (a,b)

parseLine :: Text -> (Int,Int)
parseLine = both (read . unpack) . toTuple . splitOn "/"

pieceStrength = uncurry (+)

bridgeStrength = sum . fmap pieceStrength

type Piece = (Int,Int)
type Bridge = [Piece]

fits :: Bridge -> Piece -> Bool
fits ((c,_):_) (a,b) = a == c || b == c

fitPiece bridge@((a,_):_) availablePieces = do
    fitting@(c,_) <- filter (fits bridge) availablePieces
    let newBridge = (if a == c then swap fitting else fitting) : bridge
    newBridge : (fitPiece newBridge $ delete fitting availablePieces)

initialBridge = [(0,0)]

solve = fmap (fmap swap . reverse . init) . fitPiece initialBridge . fmap parseLine . lines

-- "What is the strength of the strongest bridge you can make"
solve1 = maximum . fmap bridgeStrength . solve

-- "What is the strength of the longest bridge you can make"
solve2 = maximum . fmap snd . head . groupBy ((==) `on` fst) . reverse . sortBy (comparing fst) . fmap (length &&& bridgeStrength) . solve

solution1 = solve1 <$> input
solution2 = solve2 <$> input