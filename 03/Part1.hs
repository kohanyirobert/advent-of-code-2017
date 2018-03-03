import Day3

directions :: [Coordinate -> Coordinate]
directions = [toLeft, toUp, toRight, toDown]

findCoord :: Int -> Int -> Square -> Coordinate -> Int -> Coordinate
findCoord n size square coord step
  | n == square = coord
  | otherwise = findCoord n size newSquare newCoord newStep
  where
    index = step `div` size
    direction = directions !! index
    newSquare = square - 1
    newCoord = direction coord
    newStep = step + 1

calcDist :: Int -> Int
calcDist n = abs (ox - tx) + abs (oy - ty)
  where
    square = nextOddPerfectSquare n
    squareRoot = floor (sqrt (fromIntegral square))
    size = squareRoot - 1
    halfSize = size `div` 2
    origin = (halfSize, halfSize)
    target = findCoord n size square (size, size) 0
    (ox, oy) = origin
    (tx, ty) = target

main = do
  string <- getLine
  let number = read string :: Int
  print (calcDist number)
