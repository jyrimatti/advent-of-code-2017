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

data Instr = Set Reg RegOrVal
           | Sub Reg RegOrVal
           | Mul Reg RegOrVal
           | Jnz RegOrVal RegOrVal
  deriving Show

regP = letter

regOrValP = Left <$> regP <|> Right <$> int

instrP = try (Set <$> (string "set " *> regP)      <*> (space *> regOrValP)) <|>
         try (Sub <$> (string "sub " *> regP)      <*> (space *> regOrValP)) <|>
         try (Mul <$> (string "mul " *> regP)      <*> (space *> regOrValP)) <|>
         try (Jnz <$> (string "jnz " *> regOrValP) <*> (space *> regOrValP))

instr = either undefined id . parse instrP ""

instrs = Vec.fromList . fmap instr

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

evalProg prog muls regs loc _     | loc < 0 || loc >= length prog = (muls,regs)
evalProg _    muls regs _   steps | steps == (0::Int)             = (muls,regs)
evalProg prog muls regs loc steps =
  let
    (!m,!r,!o) = eval (muls,regs) (prog ! loc)
  in
    evalProg prog m r (loc+o) (steps-1)

stepSize = 17

solve1 inp = fst $ evalProg (instrs inp) (0::Int) (initRegs 0) 0 (-1)
solve2 inp = let 
   (_,initialRegs) = evalProg (instrs inp) (0::Int) (initRegs 1) 0 7
   start = initialRegs `index` 1
   end = initialRegs `index` 2
 in
   -- iterated 1000 (+1) times, but does not increment h for primes
   ( (end - start) `div` stepSize) + 1 - (length $ filter isPrime [start,start+stepSize..end])

solution1 = solve1 <$> input
solution2 = solve2 <$> input
