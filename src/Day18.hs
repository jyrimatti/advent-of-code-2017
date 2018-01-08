{-# LANGUAGE ScopedTypeVariables #-}
module Day18 where

import Prelude hiding (null,replicate)
import Data.Char (ord)
import Text.Parsec (parse,many1,(<|>),try)
import Text.Parsec.Char (string,letter,digit,char,space)
import Data.Vector (fromList,(!),(//),empty,null,replicate)
import qualified Control.Concurrent.Thread as Thread ( forkIO )
import Control.Concurrent.Chan (newChan,writeChan,readChan)
import Control.Concurrent.MVar (newMVar,takeMVar,tryTakeMVar,putMVar)
import Control.Exception.Base (catch,BlockedIndefinitelyOnMVar(..))
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

regP = letter

numberP = read <$> many1 (digit <|> char '-')

regOrValP = Left <$> regP <|> Right <$> numberP

instrP = try (Snd <$> (string "snd " *> regOrValP)) <|>
         try (Set <$> (string "set " *> regP) <*> (space *> regOrValP)) <|>
         try (Add <$> (string "add " *> regP) <*> (space *> regOrValP)) <|>
         try (Mul <$> (string "mul " *> regP) <*> (space *> regOrValP)) <|>
         try (Mod <$> (string "mod " *> regP) <*> (space *> regOrValP)) <|>
         try (Rcv <$> (string "rcv " *> regP)) <|>
         try (Jgz <$> (string "jgz " *> regOrValP) <*> (space *> regOrValP))

instrs = either undefined id . parse instrP ""

initRegs programID = replicate (ord 'z' - ord 'a' - 1) 0 // [(index 'p', programID)]

index r = ord r - 97

get_ regs r = regs ! index r
get regs (Left r) = regs ! index r
get _   (Right v) = v

eval _ sndQueue sends regs (Snd regOrVal) = do
  let s = get regs regOrVal
  Just prev <- tryTakeMVar sends
  putMVar sends (prev+1)
  writeChan sndQueue s
  return (regs,1)
eval _ _        _ regs (Set reg regOrVal) = return (regs // [(index reg, get regs regOrVal)],1)
eval _ _        _ regs (Add reg regOrVal) = return (regs // [(index reg, get_ regs reg + get regs regOrVal)],1)
eval _ _        _ regs (Mul reg regOrVal) = return (regs // [(index reg, get_ regs reg * get regs regOrVal)],1)
eval _ _        _ regs (Mod reg regOrVal) = return (regs // [(index reg, get_ regs reg `mod` get regs regOrVal)],1)
eval rcvQueue _ _ regs (Rcv reg) = do
  val <- readChan rcvQueue
  return (regs // [(index reg, val)],1)
eval _ _        _ regs (Jgz regOrVal os) | get regs regOrVal > 0 = return (regs,get regs os)
eval _ _        _ regs (Jgz _ _)                                 = return (regs,1)

evalProg rcvQueue sndQueue sends prog regs loc                      = do
  e <- eval rcvQueue sndQueue sends regs (prog ! loc)
  case e of
    (rs,offset) -> evalProg rcvQueue sndQueue sends prog rs (loc+offset)

solve1 inp = do
  rcvQueue <- newChan
  sndQueue <- newChan
  writeChan sndQueue 42
  sends <- newMVar 0
  let res = evalProg rcvQueue sndQueue sends (instrs <$> inp) (initRegs 0) 0
  catch res $ \BlockedIndefinitelyOnMVar -> do
    times <- takeMVar sends
    fmap last $ replicateM (times+1) $ readChan sndQueue

solve2 inp = do
    rcvQueue <- newChan
    sndQueue <- newChan
    snd1 <- newMVar 0
    snd2 <- newMVar 0
    let res1 = evalProg rcvQueue sndQueue snd1 (instrs <$> inp) (initRegs 0) 0
    let res2 = evalProg sndQueue rcvQueue snd2 (instrs <$> inp) (initRegs 1) 0
    (_,wait1) <- Thread.forkIO $ res1
    (_,wait2) <- Thread.forkIO $ res2
    catch wait1 (\BlockedIndefinitelyOnMVar -> return undefined)
    catch wait2 (\BlockedIndefinitelyOnMVar -> return undefined)
    ret1 <- takeMVar snd1
    ret2 <- takeMVar snd2
    return (ret1,ret2)
  
solution1 = solve1 =<< input
solution2 = fmap snd . solve2 =<< input