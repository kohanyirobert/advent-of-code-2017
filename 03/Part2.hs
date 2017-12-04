import Day3

import Data.Maybe

type Value = Int
type Storage = (Square, Value)
type Fragment = (Coordinate, Storage)

directions :: [Coordinate -> Coordinate]
directions = [toRight, toUp, toLeft, toDown]

toUpperLeft = toUp . toLeft
toUpperRight = toUp . toRight
toLowerLeft = toDown . toLeft
toLowerRight = toDown . toRight

neighbors :: [Coordinate -> Coordinate]
neighbors = [toRight, toUpperRight, toUp, toUpperLeft, toLeft, toLowerLeft, toDown, toLowerRight]

sumAdjacentStorages :: [Fragment] -> Coordinate -> Int
sumAdjacentStorages fragments coord = result
  where neighborFragments = map (\f -> lookup (f coord) fragments) neighbors
        result = foldl (\b a -> let (_, s) = (fromMaybe (0,0) a) in s + b) 0 neighborFragments

getFragments' :: [Fragment] -> Int -> Fragment -> Int -> [Fragment]
getFragments' fragments n fragment@(coord, (square, storage)) step
  | n <= storage = fragment : fragments
  | otherwise = getFragments' (fragment : fragments) n nextFragment nextStep
  where nextStep = step + 1
        nextSize = round (sqrt (fromIntegral nextStep)) -- https://oeis.org/A000194
        offset = nextSize - 1
        index = ((step `div` nextSize) + offset) `mod` (length directions)
        direction = directions !! index
        newCoord = direction coord
        newValue = square + 1
        newStorage = storage + (sumAdjacentStorages fragments newCoord)
        nextFragment = (newCoord, (newValue, newStorage))

getFragments :: Int -> [Fragment]
getFragments n = getFragments' [] n originFragment 0
  where square = nextOddPerfectSquare n
        root = floor (sqrt (fromIntegral square))
        size = root - 1
        halfSize = size `div` 2
        originCoordinate = (halfSize, halfSize)
        originStorage = (1, 1)
        originFragment = (originCoordinate, originStorage)

main = do
  string <- getLine
  let number = read string :: Int
  let fragments = getFragments number
  let (_, (_, storage)) = head fragments
  print storage
