import Day3

directions :: [(Int, Int) -> (Int, Int)]
directions = [toRight, toUp, toLeft, toDown]

getCoords' :: Int -> Int -> Int -> (Int, Int) -> Int -> [((Int, Int), Int)]
getCoords' n size curr coord step
  | n == curr = [(coord, curr)]
  | otherwise = (coord, curr) : getCoords' n size (curr + 1) (direction coord) (step + 1)
  where nextSize = round (sqrt (fromIntegral (step + 1))) -- https://oeis.org/A000194
        offset = nextSize - 1
        index = ((step `div` nextSize) + offset) `mod` (length directions)
        direction = directions !! index

getCoords :: Int -> [((Int, Int), Int)]
getCoords n = getCoords' n size 1 origin 0
  where square = nextOddPerfectSquare n
        root = floor (sqrt (fromIntegral square))
        size = root - 1
        halfSize = size `div` 2
        origin = (halfSize, halfSize)

main = do
  string <- getLine
  let number = read string :: Int
  print (getCoords number)
