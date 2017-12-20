module Day10 where

type Size = Int
type Sizes = [Size]
type Position = Int
type Skip = Int
type List = [Int]

data State = State Position Skip

getSizes :: String -> (Char -> Char) -> Sizes
getSizes string mapper = map read $ words $ map mapper string

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

knotHash' :: State -> Sizes -> List -> List
knotHash' state [] list = list
knotHash' (State position skip) (size : sizes) list =
  let listSize = length list
      newPositon = (position + size + skip) `mod` listSize
      newSkip = skip + 1
      newState = State newPositon newSkip
      newList = tieKnot position size list
  in knotHash' newState sizes newList

knotHash :: Sizes -> List -> List
knotHash sizes list = knotHash' (State 0 0) sizes list
