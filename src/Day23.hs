{-# LANGUAGE BangPatterns #-}
module Day23 where
    
import Prelude hiding (null,replicate,last)
import Data.Char (ord)
import Text.Parsec (parse,many1,(<|>),try)
import Text.Parsec.Char (string,letter,digit,char,space)
import Data.Vector (fromList,(!))
import Data.Numbers.Primes (isPrime)
import qualified Data.Vector.Unboxed as VU

input = lines <$> readFile "input/input23.txt"

type Reg = Char

data Instr = Set Reg (Either Reg Int)
           | Sub Reg (Either Reg Int)
           | Mul Reg (Either Reg Int)
           | Jnz (Either Reg Int) (Either Reg Int)
  deriving Show

regP = letter

numberP = read <$> many1 (digit <|> char '-')

regOrValP = Left <$> regP <|> Right <$> numberP

instrP = try (Set <$> (string "set " *> regP)      <*> (space *> regOrValP)) <|>
         try (Sub <$> (string "sub " *> regP)      <*> (space *> regOrValP)) <|>
         try (Mul <$> (string "mul " *> regP)      <*> (space *> regOrValP)) <|>
         try (Jnz <$> (string "jnz " *> regOrValP) <*> (space *> regOrValP))

instr = either undefined id . parse instrP ""

instrs = fromList . fmap instr

initRegs aValue = VU.replicate (ord 'h' - ord 'a' + 1) 0 VU.// [(index 'a', aValue)]

index r = ord r - 97

get_ regs r = regs VU.! index r
get regs (Left r) = regs VU.! index r
get _   (Right v) = v

eval :: (Int,VU.Vector Int) -> Instr -> (Int,VU.Vector Int,Int)
eval (muls,regs) (Set reg regOrVal)                         = (muls, regs VU.// [(index reg, get regs regOrVal)],1)
eval (muls,regs) (Sub reg regOrVal)                         = (muls, regs VU.// [(index reg, get_ regs reg - get regs regOrVal)],1)
eval (muls,regs) (Mul reg regOrVal)                         = (muls+1, regs VU.// [(index reg, get_ regs reg * get regs regOrVal)],1)
eval (muls,regs) (Jnz regOrVal os) | get regs regOrVal /= 0 = (muls,regs,get regs os)
eval (muls,regs) (Jnz _ _)                                  = (muls,regs,1)

evalProg prog muls regs loc steps | loc < 0 || loc >= length prog ||steps == (0::Int) = (muls,regs)
evalProg prog muls regs loc steps =
  let
    (!m,!r,!o) = eval (muls,regs) (prog ! loc)
  in
    evalProg prog m (VU.force r) (loc+o) (steps-1)

stepSize = 17

solve1 inp = evalProg (instrs inp) 0 (initRegs 0) 0 (-1)
solve2 inp = let 
   (_,initialRegs) = evalProg (instrs inp) 0 (initRegs 1) 0 7
   start = initialRegs VU.! 1
   end = initialRegs VU.! 2
 in
   -- iterated 1000 (+1) times, but does not increment h for primes
   ( (end - start) `div` stepSize) + 1 - (length $ filter isPrime [start,start+stepSize..end])

solution1 = fmap fst $ solve1 <$> input

solution2 = solve2 <$> input
