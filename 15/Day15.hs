module Day15 where

import Data.Bits ((.&.))
import Data.Char (isDigit)

type Pair = (Int, Int)

divisor :: Int
divisor = 2147483647

factorA, factorB :: Int
factorA = 16807

factorB = 48271

getFirstPair :: String -> Pair
getFirstPair string = (a, b)
  where
    (a:b:_) = map (read . filter isDigit) $ lines string

isMatch :: Pair -> Bool
isMatch (a, b) = a .&. 0xffff == b .&. 0xffff

generator :: Int -> Int -> [Int]
generator factor start = iterate (\next -> next * factor `mod` divisor) start

getPairs :: Int -> [Int] -> [Int] -> [Pair]
getPairs n as bs = take n $ zip as bs
