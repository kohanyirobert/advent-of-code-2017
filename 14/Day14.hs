module Day14 where

import qualified Data.Map.Strict as Map

import Data.Char (isControl)
import Day10 (Encoding(Binary), knotHash)

type Region = Int

data State = Free | Used Region
  deriving (Eq, Show)

type Count = Int
type Coodinate = (Int, Int)
type Grid = Map.Map Coodinate State

isUsed :: State -> Bool
isUsed Free = False
isUsed (Used _) = True

getGrid :: String -> Grid
getGrid string = Map.fromList $ concat indexedGrid
  where range = [0..127]
        key = filter (not . isControl) string
        binaryHashes = map (\i -> knotHash Binary (key ++ "-" ++ show i)) range
        toRow = \binaryHash -> map (\char -> if char == '0' then Free else Used 0) binaryHash
        grid = map toRow binaryHashes
        indexedGrid = map (\(i, row) -> map (\(j, state) -> ((i, j), state)) $ zip range row) $ zip range grid

countUsed :: Grid -> Count
countUsed grid = foldl (\count s -> count + if isUsed s then 1 else 0) 0 grid
