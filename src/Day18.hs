module Day18 where

import Data.Char (ord)
import qualified Data.Sequence as Seq (fromList)
import Data.Sequence (index,update)

import Control.Monad (replicateM)
import Control.Exception.Base (catch,BlockedIndefinitelyOnMVar(..))
import Control.Concurrent.Thread (forkIO)
import Control.Concurrent.Chan (newChan,writeChan,readChan)
import Control.Concurrent.MVar (newMVar,takeMVar,tryTakeMVar,putMVar)

import Text.Parsec (parse,(<|>),try)
import Text.Parsec.Char (string,letter,space)
import Text.ParserCombinators.Parsec.Number (int)

input       = Seq.fromList <$> lines <$> readFile "input/input18.txt"
input_test  = Seq.fromList <$> lines <$> readFile "input/input18_test.txt"
input_test2 = Seq.fromList <$> lines <$> readFile "input/input18_test2.txt"

type Reg      = Char
type RegOrInt = Either Reg Int

data Instr = Snd RegOrInt
           | Set Reg RegOrInt
           | Add Reg RegOrInt
           | Mul Reg RegOrInt
           | Mod Reg RegOrInt
           | Rcv Reg
           | Jgz RegOrInt RegOrInt
  deriving Show

regP = letter

regOrValP = Left <$> regP <|> Right <$> int

instrP = try (Snd <$> (string "snd " *> regOrValP))                     <|>
         try (Set <$> (string "set " *> regP) <*> (space *> regOrValP)) <|>
         try (Add <$> (string "add " *> regP) <*> (space *> regOrValP)) <|>
         try (Mul <$> (string "mul " *> regP) <*> (space *> regOrValP)) <|>
         try (Mod <$> (string "mod " *> regP) <*> (space *> regOrValP)) <|>
         try (Rcv <$> (string "rcv " *> regP))                          <|>
         try (Jgz <$> (string "jgz " *> regOrValP) <*> (space *> regOrValP))

instrs = either undefined id . parse instrP ""

indexOf r = ord r - 97

initRegs programID = update (indexOf 'p') programID $ Seq.fromList $ fmap (const 0) ['a'..'z']

get_ regs r       = regs `index` indexOf r
get regs (Left r) = regs `index` indexOf r
get _   (Right v) = v

eval _ sndQueue sends regs (Snd regOrVal) = do
  let value = get regs regOrVal
  Just prev <- tryTakeMVar sends
  putMVar sends (prev+1)
  writeChan sndQueue value
  return (regs,1)
eval _ _        _ regs (Set reg regOrVal) = return (update (indexOf reg) (get regs regOrVal) regs,1)
eval _ _        _ regs (Add reg regOrVal) = return (update (indexOf reg) (get_ regs reg + get regs regOrVal) regs,1)
eval _ _        _ regs (Mul reg regOrVal) = return (update (indexOf reg) (get_ regs reg * get regs regOrVal) regs,1)
eval _ _        _ regs (Mod reg regOrVal) = return (update (indexOf reg) (get_ regs reg `mod` get regs regOrVal) regs,1)
eval rcvQueue _ _ regs (Rcv reg) = do
  val <- readChan rcvQueue
  return (update (indexOf reg) val regs,1)
eval _ _        _ regs (Jgz regOrVal offset) | get regs regOrVal > 0 = return (regs,get regs offset)
eval _ _        _ regs (Jgz _ _)                                     = return (regs,1)

evalProg rcvQueue sndQueue sends prog regs loc = do
  (newRegs,offset) <- eval rcvQueue sndQueue sends regs (prog `index` loc)
  evalProg rcvQueue sndQueue sends prog newRegs (loc+offset)

-- "What is the value of the recovered frequency the first time a rcv instruction is executed with a non-zero value"
solve1 inp = do
  rcvQueue <- newChan
  sndQueue <- newChan
  writeChan sndQueue undefined
  sends <- newMVar 0
  let res = evalProg rcvQueue sndQueue sends (fmap instrs inp) (initRegs 0) 0
  -- will block on first rcv, since no one is sending anything
  catch res $ \BlockedIndefinitelyOnMVar -> do
    times <- takeMVar sends
    -- read as many sounds from the queue as have been played, return the last one.
    fmap last $ replicateM (times+1) $ readChan sndQueue

-- "how many times did program 1 send a value"
solve2 inp = do
    rcvQueue <- newChan
    sndQueue <- newChan
    snd1 <- newMVar (0::Int)
    snd2 <- newMVar (0::Int)
    -- receive queue for the second process is the send queue of the first one, and the other way round.
    let res1 = evalProg rcvQueue sndQueue snd1 (fmap instrs inp) (initRegs 0) 0
    let res2 = evalProg sndQueue rcvQueue snd2 (fmap instrs inp) (initRegs 1) 0
    (_,wait1) <- forkIO $ res1
    (_,wait2) <- forkIO $ res2
    catch wait1 (\BlockedIndefinitelyOnMVar -> return undefined)
    catch wait2 (\BlockedIndefinitelyOnMVar -> return undefined)
    -- after both have finished, return number of sends for each process
    ret1 <- takeMVar snd1
    ret2 <- takeMVar snd2
    return (ret1,ret2)
  
solution1 = solve1 =<< input
solution2 = fmap snd . solve2 =<< input