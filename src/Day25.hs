{-# LANGUAGE BangPatterns #-}
module Day25 where

import Prelude hiding (replicate,length,(++))
import Text.Parsec (parse,many,(<|>),try)
import Text.Parsec.Char (string,noneOf,anyChar,char,space)
import Data.List (find)
import Data.Foldable (toList)
import Data.Sequence (update,replicate,length,index,(|>))

input = readFile "input/input25.txt"
input_test = readFile "input/input25_test.txt"

data Instructions = Instructions {
    beginState :: Char,
    endInSteps :: Int,
    states :: [State]
} deriving Show

data State = State {
    name :: Char,
    phase0 :: Phase,
    phase1 :: Phase
} deriving Show

data Phase = Phase {
    condition :: Int,
    output :: Int,
    direction :: Int,
    nextState :: Char
} deriving Show

ws = many space

header = string "Begin in state " *> anyChar <* char '.'
endCondition = read <$> (string "Perform a diagnostic checksum after " *> many (noneOf " ") <* string " steps.")

phaseHeader = string "In state " *> anyChar <* char ':'
cond = read <$> (string "If the current value is " *> many (noneOf ":") <* char ':')
write = read <$> (string "- Write the value " *> many (noneOf ".") <* char '.')
moveRight = string "- Move one slot to the right." *> pure 1
moveLeft = string "- Move one slot to the left." *> pure (-1)
continue = string "- Continue with state " *> anyChar <* char '.'

phase = Phase <$> (cond <* ws) <*> (write <* ws) <*> ((try moveRight <|> try moveLeft) <* ws) <*> (continue <* ws)

state = State <$> (phaseHeader <* ws) <*> phase <*> phase

instructions = Instructions <$> (header <* ws) <*> (endCondition <* ws) <*> many (state <* ws)

instrs = either undefined id <$> parse instructions ""

extend loc vec | length vec <= abs loc + 1 = vec |> 0
extend _   vec                             = vec

run 0          (neg,pos)      _   _     _ = (reverse $ tail $ toList neg, toList pos)
run iterations (neg,pos) loc s ss = let
    Just (State _ p0 p1) = find ((== s) . name) ss
    !curValue = if loc < 0 then neg `index` abs loc else pos `index` loc
    Phase _ out dir newState = if curValue == 0 then p0 else p1
    !newTape = (if loc < 0 then update (abs loc) out (extend loc neg) else neg,
                if loc >= 0 then update loc out (extend loc pos) else pos)
    !newLoc = loc + dir
  in
    run (iterations-1) newTape newLoc newState ss

checksum = sum

solve (Instructions begin endIn ss) = run endIn (replicate 2 0,replicate 2 0) 0 begin ss

solve1 = (\(a,b) -> checksum a + checksum b) . solve . instrs

solution1 = solve1 <$> input