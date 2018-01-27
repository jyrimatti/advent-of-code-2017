{-# LANGUAGE BangPatterns #-}
module Day23 where
    
import Data.Char (ord)
import Data.Numbers.Primes (isPrime)
import qualified Data.Vector as Vec (fromList)
import Data.Vector ((!))
import qualified Data.Sequence as Seq (fromList)
import Data.Sequence (index,update)

import Text.Parsec (parse,(<|>),try)
import Text.Parsec.Char (string,letter,space)
import Text.ParserCombinators.Parsec.Number (int)

input = lines <$> readFile "input/input23.txt"

type Reg = Char
type RegOrVal = Either Reg Int

data Instruction = Set Reg RegOrVal
                 | Sub Reg RegOrVal
                 | Mul Reg RegOrVal
                 | Jnz RegOrVal RegOrVal
  deriving Show

regP = letter

regOrValP = Left <$> regP <|> Right <$> int

instructionP = try (Set <$> (string "set " *> regP)      <*> (space *> regOrValP)) <|>
               try (Sub <$> (string "sub " *> regP)      <*> (space *> regOrValP)) <|>
               try (Mul <$> (string "mul " *> regP)      <*> (space *> regOrValP)) <|>
               try (Jnz <$> (string "jnz " *> regOrValP) <*> (space *> regOrValP))

instruction = either undefined id . parse instructionP ""

instructions = Vec.fromList . fmap instruction

indexOf r = ord r - 97

initRegs aValue = update (indexOf 'a') aValue $ Seq.fromList $ fmap (const 0) ['a'..'h']

get_ regs r       = regs `index` indexOf r
get regs (Left r) = regs `index` indexOf r
get _   (Right v) = v

eval (muls,regs) (Set reg regOrVal)                         = (muls  , update (indexOf reg) (get regs regOrVal) regs, 1)
eval (muls,regs) (Sub reg regOrVal)                         = (muls  , update (indexOf reg) (get_ regs reg - get regs regOrVal) regs, 1)
eval (muls,regs) (Mul reg regOrVal)                         = (muls+1, update (indexOf reg) (get_ regs reg * get regs regOrVal) regs, 1)
eval (muls,regs) (Jnz regOrVal os) | get regs regOrVal /= 0 = (muls  , regs, get regs os)
eval (muls,regs) (Jnz _ _)                                  = (muls  , regs, 1)

evalProg program muls regs location _     | location < 0 || location >= length program = (muls,regs)
evalProg _       muls regs _        stepsLeft | stepsLeft == (0::Int)                  = (muls,regs)
evalProg program muls regs location stepsLeft =
  let
    (!newMuls,!newRegs,!offset) = eval (muls,regs) (program ! location)
  in
    evalProg program newMuls newRegs (location+offset) (stepsLeft-1)

stepSize = 17

-- "how many times is the mul instruction invoked"
solve1 inp = fst $ evalProg (instructions inp) (0::Int) (initRegs 0) 0 (-1)

-- "what value would be left in register h"
solve2 inp = let 
   (_,initialRegs) = evalProg (instructions inp) (0::Int) (initRegs 1) 0 7
   start = initialRegs `index` 1
   end = initialRegs `index` 2
 in
   -- iterated 1000 (+1) times, but does not increment h for primes
   ( (end - start) `div` stepSize) + 1 - (length $ filter isPrime [start,start+stepSize..end])

solution1 = solve1 <$> input
solution2 = solve2 <$> input
