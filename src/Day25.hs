{-# LANGUAGE BangPatterns #-}
module Day25 where

import Prelude hiding (replicate)
import Data.Foldable (toList)
import Data.Tuple.Extra (both)
import Data.Bifunctor (bimap,first,second)
import Data.List (find)
import Data.Sequence (Seq,update,replicate,index,(|>))

import Text.Parsec (parse,many,(<|>),try)
import Text.Parsec.Char (string,noneOf,anyChar,char,space)

input      = readFile "input/input25.txt"
input_test = readFile "input/input25_test.txt"

data Instructions = Instructions {
    _beginStateName :: Char,
    _endInSteps     :: Int,
    _states         :: [State]
} deriving Show

data State = State {
    _name   :: Char,
    _phase0 :: Phase,
    _phase1 :: Phase
} deriving Show

data Phase = Phase {
    _condition     :: Int,
    _output        :: Int,
    _direction     :: Int,
    _nextStateName :: Char
} deriving Show

ws = many space

headerP       = string "Begin in state " *> anyChar <* char '.'
endConditionP = read <$> (string "Perform a diagnostic checksum after " *> many (noneOf " ") <* string " steps.")

phaseHeaderP = string "In state " *> anyChar <* char ':'
condP        = read <$> (string "If the current value is " *> many (noneOf ":") <* char ':')
writeP       = read <$> (string "- Write the value " *> many (noneOf ".") <* char '.')
moveRightP   = string "- Move one slot to the right." *> pure 1
moveLeftP    = string "- Move one slot to the left." *> pure (-1)
continueP    = string "- Continue with state " *> anyChar <* char '.'

phaseP = Phase <$> (condP <* ws) <*> (writeP <* ws) <*> ((try moveRightP <|> try moveLeftP) <* ws) <*> (continueP <* ws)

stateP = State <$> (phaseHeaderP <* ws) <*> phaseP <*> phaseP

instructionsP = Instructions <$> (headerP <* ws) <*> (endConditionP <* ws) <*> many (stateP <* ws)

instructions = either undefined id . parse instructionsP ""

extendIfNeeded requiredPosition sequ | length sequ <= abs requiredPosition + 1 = sequ |> 0
extendIfNeeded _                sequ                                           = sequ

type Tape = (Seq Int,Seq Int)

(!!!) :: Tape -> Int -> Int
(neg,_  ) !!! position | position < 0 = neg `index` abs position
(_  ,pos) !!! position                = pos `index` position

updateTape position value | position < 0 = first  $ update (abs position) value . extendIfNeeded position
updateTape position value                = second $ update      position  value . extendIfNeeded position

run 0         tape _               _                _      = bimap (reverse . tail . toList) toList tape
run stepsLeft tape currentPosition currentStateName states = let
    Just (State _ phase0 phase1)          = find ((== currentStateName) . _name) states
    Phase _ output direction newStateName = if tape !!! currentPosition == 0 then phase0 else phase1
    !newTape                              = updateTape currentPosition output tape
    !newPosition                          = currentPosition + direction
  in
    run (stepsLeft-1) newTape newPosition newStateName states

checksum = sum

solve (Instructions beginStateName endInSteps states) = run endInSteps (replicate 2 0, replicate 2 0) 0 beginStateName states

-- "What is the diagnostic checksum"
solve1 = uncurry (+) . both checksum . solve . instructions

solution1 = solve1 <$> input