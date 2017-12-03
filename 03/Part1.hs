import Day3

directions :: [(Int, Int) -> (Int, Int)]
directions = [toLeft, toUp, toRight, toDown]

findCoord :: Int -> Int -> Int -> (Int, Int) -> Int -> (Int, Int)
findCoord n size curr coord step
  | n == curr = coord
  | otherwise = findCoord n size (curr - 1) (direction coord) (step + 1)
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

main = do
  string <- getLine
  let number = read string :: Int
  print (calcDist number)
