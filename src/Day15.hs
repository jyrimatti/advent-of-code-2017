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

calculate mult prev = prev * mult `rem` divisor

calc condition factor initial = takeEnd 16 . pack . toBinary <$> (filter condition $ iterate (calculate factor) initial)

calcBoth a b = zip (calc (const True) factorA a) (calc (const True) factorB b)

calcBoth2 aa b = zip (calc (\a -> (a `mod` 4) == 0) factorA aa) (calc (\a -> (a `mod` 8) == 0) factorB b)

solve1 a b = length $ filter (uncurry (==)) $ take 40000000 $ calcBoth a b

solve2 a b = length $ filter (uncurry (==)) $ take 5000000 $ calcBoth2 a b

solution1 = solve1 inputA inputB

solution2 = solve2 inputA inputB