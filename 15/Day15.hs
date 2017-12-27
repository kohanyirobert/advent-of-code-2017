module Day15 where

import Data.Char (isDigit)
import Data.Bits ((.&.))

type Pair = (Int, Int)

getFirstPair :: String -> Pair
getFirstPair string = (a, b)
  where (a : b : _) = map (read . filter isDigit) $ lines string

isMatch:: Pair -> Bool
isMatch (a, b) = a .&. 0xffff == b .&. 0xffff

generate :: Int -> Int -> [Int]
generate factor start = iterate (\next -> next * factor `mod` 2147483647) start

getPairs :: Int -> Pair -> [Pair]
getPairs n (a, b) = take n $ zip as bs
  where as = generate 16807 a
        bs = generate 48271 b
