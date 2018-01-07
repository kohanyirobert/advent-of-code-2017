module Day19 where

import qualified Data.Map.Strict as Map

import Data.Maybe (isJust)

type Coordinate = (Int, Int)
type Diagram = Map.Map Coordinate Field
type Steps = Int

data Direction = Upward | Downward | Leftward | Rightward
  deriving (Eq, Show)

data Packet = Packet Coordinate Direction [Char] Steps
  deriving (Eq, Show)

data Field = Blank | Turn | Vertical | Horizontal | Path Char
  deriving Show

toField :: Char -> Field
toField c
  | c == ' ' = Blank
  | c == '+' = Turn
  | c == '|' = Vertical
  | c == '-' = Horizontal
  | otherwise = Path c

isBlank :: Field -> Bool
isBlank Blank = True
isBlank _ = False

isStart :: Coordinate -> Field -> Bool
isStart (_, y) Vertical = y == 0
isStart _ _ = False

isVertical :: Direction -> Bool
isVertical Upward = True
isVertical Downward = True
isVertical _ = False

isHorizontal :: Direction -> Bool
isHorizontal Leftward = True
isHorizontal Rightward = True
isHorizontal _ = False

getDiagram string
  = let rows = lines string
        indexedRows = zip [0..(length rows) - 1] rows
        rowMapper = \(y, row) -> let cells = row
                                     indexedCells = zip [0..(length cells) - 1] cells
                                     cellMapper = \(x, cell) -> ((x, y), toField cell)
                         in map cellMapper indexedCells
    in Map.filter (not . isBlank) . Map.fromList . concat . map rowMapper $ indexedRows

findStart :: Diagram -> Coordinate
findStart diagram = fst . head . Map.toList . Map.filterWithKey isStart $ diagram

makePacket :: Coordinate -> Packet
makePacket coordinate = Packet coordinate Downward [] 0

nextCoordinate :: Direction -> Coordinate -> Coordinate
nextCoordinate Upward (x, y) = (x, y - 1)
nextCoordinate Downward (x, y) = (x, y + 1)
nextCoordinate Leftward (x, y) = (x - 1, y)
nextCoordinate Rightward (x, y) = (x + 1, y)

findDirection :: Diagram -> Direction -> Coordinate -> Field -> Direction
findDirection diagram direction coordinate Turn
  | isVertical direction && isJust leftward = Leftward
  | isVertical direction && isJust rightward = Rightward
  | isHorizontal direction && isJust upward = Upward
  | isHorizontal direction && isJust downward = Downward
  where upward = Map.lookup (nextCoordinate Upward coordinate) diagram
        downward = Map.lookup (nextCoordinate Downward coordinate) diagram
        leftward = Map.lookup (nextCoordinate Leftward coordinate) diagram
        rightward = Map.lookup (nextCoordinate Rightward coordinate) diagram
findDirection _ direction _ _ = direction

updateChars :: Field -> [Char] -> [Char]
updateChars (Path c) chars = chars ++ [c]
updateChars _ chars = chars

movePacket :: Diagram -> Packet -> Packet
movePacket diagram packet@(Packet coordinate direction chars steps)
  | Map.member coordinate diagram = Packet coordinate' direction' chars' steps'
  | otherwise = packet
  where (Just field) = Map.lookup coordinate diagram
        chars' = updateChars field chars
        direction' = findDirection diagram direction coordinate field
        coordinate' = nextCoordinate direction' coordinate
        steps' = steps + 1

followPath :: Diagram -> Packet -> Packet
followPath diagram packet
  | packet == packet' = packet
  | otherwise = followPath diagram packet'
  where packet' = movePacket diagram packet
