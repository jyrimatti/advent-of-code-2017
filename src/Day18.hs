{-# LANGUAGE ScopedTypeVariables #-}
module Day18 where

import Prelude hiding (null,replicate)
import Data.Char (ord,chr)
import Text.Parsec (parse,many1,option,(<|>),try)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (string,noneOf,anyChar,letter,digit,char,space)
import Text.Parsec.Combinator (sepBy1)
import Data.Vector (fromList,(!),(//),empty,null,replicate)
import Control.Concurrent (forkIO)
import qualified Control.Concurrent.Thread as Thread ( forkIO, result )
import Control.Concurrent.Chan (newChan,writeChan,readChan,getChanContents)
import Control.Concurrent.MVar (newMVar,takeMVar,tryTakeMVar,putMVar)
import Control.Exception.Base (catch,Exception,BlockedIndefinitelyOnMVar(..))
import Control.Monad (replicateM)

input = fromList <$> lines <$> readFile "input/input18.txt"
input_test = fromList <$> lines <$> readFile "input/input18_test.txt"
input_test2 = fromList <$> lines <$> readFile "input/input18_test2.txt"

type Reg = Char

data Instr = Snd (Either Reg Int)
           | Set Reg (Either Reg Int)
           | Add Reg (Either Reg Int)
           | Mul Reg (Either Reg Int)
           | Mod Reg (Either Reg Int)
           | Rcv Reg
           | Jgz (Either Reg Int) (Either Reg Int)
  deriving Show

reg :: Parser Reg
reg = letter

number :: Parser Int
number = read <$> many1 (digit <|> char '-')

regOrVal :: Parser (Either Reg Int)
regOrVal = Left <$> reg <|> Right <$> number

instrP :: Parser Instr
instrP = try (Snd <$> (string "snd " *> regOrVal)) <|>
         try (Set <$> (string "set " *> reg) <*> (space *> regOrVal)) <|>
         try (Add <$> (string "add " *> reg) <*> (space *> regOrVal)) <|>
         try (Mul <$> (string "mul " *> reg) <*> (space *> regOrVal)) <|>
         try (Mod <$> (string "mod " *> reg) <*> (space *> regOrVal)) <|>
         try (Rcv <$> (string "rcv " *> reg)) <|>
         try (Jgz <$> (string "jgz " *> regOrVal) <*> (space *> regOrVal))

instrs input = either undefined id <$> parse instrP "" <$> input

initRegs programID = replicate (ord 'z' - ord 'a' - 1) 0 // [(index 'p', programID)]

index r = ord r - 97

get_ regs r = regs ! index r
get regs (Left r) = regs ! index r
get regs (Right v) = v

eval _ sndQueue snd (lp,regs) (Snd regOrVal)                                 = do
  let s = get regs regOrVal
  Just prev <- tryTakeMVar snd
  putMVar snd (prev+1)
  writeChan sndQueue s
  return (s,regs,1)
eval _ _        _ (lp,regs) (Set reg regOrVal)                        = return (lp, regs // [(index reg, get regs regOrVal)],1)
eval _ _        _ (lp,regs) (Add reg regOrVal)                        = return (lp, regs // [(index reg, get_ regs reg + get regs regOrVal)],1)
eval _ _        _ (lp,regs) (Mul reg regOrVal)                        = return (lp, regs // [(index reg, get_ regs reg * get regs regOrVal)],1)
eval _ _        _ (lp,regs) (Mod reg regOrVal)                        = return (lp, regs // [(index reg, get_ regs reg `mod` get regs regOrVal)],1)
eval rcvQueue _ _ (lp,regs) (Rcv reg)                                 = do
  val <- readChan rcvQueue
  return (lp,regs // [(index reg, val)],1)
eval _ _        _ (lp,regs) (Rcv _)                                   = return (lp,empty,1) 
eval _ _        _ (lp,regs) (Jgz regOrVal os) | get regs regOrVal > 0 = return (lp,regs,get regs os)
eval _ _        _ (lp,regs) (Jgz _ _)                                 = return (lp,regs,1)

evalProg rcvQueue sndQueue snd prog lp regs loc                      = do
  e <- eval rcvQueue sndQueue snd (lp,regs) (prog ! loc)
  case e of
    (lp,regs,_) | null regs -> return lp
    (lp,regs,offset)        -> evalProg rcvQueue sndQueue snd prog lp regs (loc+offset)

solve1 input = do
  rcvQueue <- newChan
  sndQueue <- newChan
  writeChan sndQueue 42
  snd <- newMVar 0
  let res = evalProg rcvQueue sndQueue snd (instrs input) (-1) (initRegs 0) 0
  catch res $ \BlockedIndefinitelyOnMVar -> do
    times <- takeMVar snd
    fmap last $ replicateM (times+1) $ readChan sndQueue

solve2 input = do
    rcvQueue <- newChan
    sndQueue <- newChan
    snd1 <- newMVar 0
    snd2 <- newMVar 0
    let res1 = evalProg rcvQueue sndQueue snd1 (instrs input) (-1) (initRegs 0) 0
    let res2 = evalProg sndQueue rcvQueue snd2 (instrs input) (-1) (initRegs 1) 0
    (_,wait1) <- Thread.forkIO $ res1
    (_,wait2) <- Thread.forkIO $ res2
    catch wait1 (\BlockedIndefinitelyOnMVar -> return $ Right 42)
    catch wait2 (\BlockedIndefinitelyOnMVar -> return $ Right 42)
    ret1 <- takeMVar snd1
    ret2 <- takeMVar snd2
    return (ret1,ret2)
  
solution1 = solve1 =<< input
solution2 = fmap snd $ solve2 =<< input