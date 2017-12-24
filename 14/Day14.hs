module Day14 where

import qualified Data.Map.Strict as Map

import Data.Char (isControl)
import Day10 (Encoding(Binary), knotHash)

data State = Free | Used
  deriving (Eq, Show)

type Count = Int
type Coodinate = (Int, Int)
type Grid = Map.Map Coodinate State

getGrid :: String -> Grid
getGrid string = Map.fromList $ concat indexedGrid
  where range = [0..127]
        key = filter (not . isControl) string
        binaryHashes = map (\i -> knotHash Binary (key ++ "-" ++ show i)) range
        toRow = \binaryHash -> map (\char -> if char == '0' then Free else Used) binaryHash
        grid = map toRow binaryHashes
        indexedGrid = map (\(i, row) -> map (\(j, state) -> ((i, j), state)) $ zip range row) $ zip range grid

countState :: State -> Grid -> Count
countState state grid = foldl (\count s -> count + if s == state then 1 else 0) 0 grid
