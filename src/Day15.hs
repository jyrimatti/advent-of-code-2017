{-# LANGUAGE FlexibleContexts #-}
module Day15 where
  
import Data.Char (intToDigit)
import Data.Text (pack, takeEnd)
import Numeric (showIntAtBase)

inputA = 618
inputB = 814

factorA = 16807
factorB = 48271
divisor = 2147483647

testA = 65 :: Int
testB = 8921 :: Int

toBinary :: Int -> String
toBinary = flip (showIntAtBase 2 intToDigit) ""

calculate factor previous = previous * factor `rem` divisor

generate condition factor initial = takeEnd 16 . pack . toBinary <$> (filter condition $ iterate (calculate factor) initial)

noFilter = const True
divisibleBy x = (0 ==) . (`mod` x)

generatePairs (a,b) = zip (generate noFilter factorA a) (generate noFilter factorB b)

generatePairs2 (a,b) = zip (generate (divisibleBy 4) factorA a) (generate (divisibleBy 8) factorB b)

-- "what is the judge's final count"
solve1 = length . filter (uncurry (==)) . take 40000000 . generatePairs

-- "what is the judge's final count"
solve2 = length . filter (uncurry (==)) . take 5000000 . generatePairs2

solution1 = solve1 (inputA, inputB)

solution2 = solve2 (inputA, inputB)