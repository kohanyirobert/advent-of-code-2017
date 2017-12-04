module Day3 where

{-
  0,0    1,0      ...    s-1,0    s,0
  0,1    1,1             s-1,1    s,1

   .                               .
   .            s/2,s/2            .
   .                               .

  0,s-1  1,s-1           s-1,s-1  s,s-1
  0,s    1,s      ...    s-1,s    s,s
-}

type Coordinate = (Int, Int)
type Square = Int

toLeft (x, y) = (x - 1, y)
toUp (x, y) = (x, y - 1)
toRight (x, y) = (x + 1, y)
toDown (x, y) = (x, y + 1)

nextOddPerfectSquare :: Int -> Int
nextOddPerfectSquare n
  | odd n && squareRoot^2 == n = n
  | otherwise = nextOddPerfectSquare (n + 1)
  where squareRoot = floor (sqrt (fromIntegral n))
