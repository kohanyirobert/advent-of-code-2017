module Day14 where

import qualified Data.Map.Strict as Map

import Data.Char (isControl)
import Data.Maybe (isJust, fromJust)
import Day10 (Encoding(Binary), knotHash)

type Region = Int
type Count = Int
type Coordinate = (Int, Int)
type Grid = Map.Map Coordinate State

data State = Free | Used Region
  deriving (Ord, Eq, Show)

range :: [Int]
range = [0..127]

isUsed :: State -> Bool
isUsed Free = False
isUsed _ = True

getGrid :: String -> Grid
getGrid string = Map.fromList $ concat indexedGrid
  where key = filter (not . isControl) string
        binaryHashes = map (\i -> knotHash Binary (key ++ "-" ++ show i)) range
        toRow = \binaryHash -> map (\char -> if char == '0' then Free else Used 0) binaryHash
        grid = map toRow binaryHashes
        indexedGrid = map (\(i, row) -> map (\(j, state) -> ((i, j), state)) $ zip range row) $ zip range grid

countUsed :: Grid -> Count
countUsed grid = length . Map.filter isUsed $ grid

updateAdjacents :: Region -> Coordinate -> Grid -> Grid
updateAdjacents region coordinate@(x, y) grid
  | adjacentCoordinates == [] = nextGrid
  | otherwise = foldl (\newGrid adjacentCoordinate -> updateAdjacents region adjacentCoordinate newGrid) nextGrid adjacentCoordinates
  where newState = Used region
        nextGrid = Map.update (\_ -> Just newState) coordinate grid
        topNeighbor = (x, y - 1)
        rightNeighbor = (x + 1, y)
        bottomNeighbor = (x, y + 1)
        leftNeighbor = (x - 1, y)
        allNeighbors = [topNeighbor, rightNeighbor, bottomNeighbor, leftNeighbor]
        usedNeighbors = Map.filter isUsed $ Map.filterWithKey (\k _ -> k `elem` allNeighbors) nextGrid
        unseenNeighbors = Map.toList $ Map.filter (\(Used region) -> region == 0) usedNeighbors
        adjacentCoordinates = map (\(adjacentCoordinate, _) -> adjacentCoordinate) unseenNeighbors

updateRegions' :: Region -> Grid -> Grid
updateRegions' region grid
  | filtered == [] = grid
  | otherwise = updateRegions' nextRegion newGrid
  where filtered = Map.toList $ Map.filter (\(Used region) -> region == 0) $ Map.filter isUsed grid
        next = head filtered
        (coordinate, _) = next
        newGrid = updateAdjacents region coordinate grid
        nextRegion = region + 1

updateRegions :: Grid -> Grid
updateRegions grid = updateRegions' 1 grid

countRegions :: Grid -> Count
countRegions grid = region
  where (Used region) = maximum . Map.elems . Map.filter isUsed $ grid
