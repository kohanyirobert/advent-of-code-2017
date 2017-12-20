module Day10 where

import Data.Char (ord)

type Size = Int
type Sizes = [Size]
type Position = Int
type Skip = Int
type List = [Int]
type Times = Int

data State = State Position Skip List

getSizes :: String -> Sizes
getSizes string =
  let commaToSpace = \c -> if c == ',' then ' ' else c
  in map read $ words $ map commaToSpace string

getByteSizes :: String -> Sizes
getByteSizes string = map ord $ head $ lines string

tieKnot :: Position -> Size -> List -> List
tieKnot start skip list
  | wrap = let end = (start + normalizedSkip) `mod` size
               (head, temp) = splitAt end list
               (middle, tail) = splitAt (start - end) temp
               sub = reverse (tail ++ head)
               (newTail, newHead) = splitAt (length tail) sub
               in newHead ++ middle ++ newTail
  | otherwise = let end = normalizedSkip `mod` size
                    (head, temp) = splitAt start list
                    (middle, tail) = splitAt end temp
                in head ++ (reverse middle) ++ tail
  where size = length list
        normalizedSkip = skip `mod` size
        wrap = start + normalizedSkip >= size

knotHash' :: Sizes -> State -> State
knotHash' [] state = state
knotHash' (size : sizes) (State position skip list) =
  let listSize = length list
      newPositon = (position + size + skip) `mod` listSize
      newSkip = skip + 1
      newList = tieKnot position size list
      newState = State newPositon newSkip newList
  in knotHash' sizes newState

knotHash :: Times -> Sizes -> List -> List
knotHash times sizes list = result
  where (State _ _ result) = foldl (\state _ -> knotHash' sizes state) (State 0 0 list) [0..times - 1]
