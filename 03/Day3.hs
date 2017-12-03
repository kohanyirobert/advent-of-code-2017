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

toLeft (x, y) = (x - 1, y)
toUp (x, y) = (x, y - 1)
toRight (x, y) = (x + 1, y)
toDown (x, y) = (x, y + 1)

directions :: [(Int, Int) -> (Int, Int)]
directions = [toLeft, toUp, toRight, toDown]

nextOddPerfectSquare :: Int -> Int
nextOddPerfectSquare n
  | odd n && root^2 == n = n
  | otherwise = nextOddPerfectSquare (n + 1)
  where root = floor (sqrt (fromIntegral n))

findCoord :: Int -> Int -> Int -> (Int, Int) -> Int -> (Int, Int)
findCoord n size curr coord step
  | n == curr = coord
  | otherwise = findCoord n size (curr -1) (direction coord) (step + 1)
  where index = step `div` size
        direction = directions !! index

calcDist n = abs (ox - tx) + abs (oy - ty)
  where square = nextOddPerfectSquare n
        root = floor (sqrt (fromIntegral square))
        size = root - 1
        halfSize = size `div` 2
        origin = (halfSize, halfSize)
        target = findCoord n size square (size, size) 0
        (ox, oy) = origin
        (tx, ty) = target
