module Day14 where

import Day10 (Encoding(Binary), knotHash)

import Data.Char (isControl)

data State = Free | Used
  deriving (Eq, Show)

type Count = Int
type Grid = [[State]]

getGrid :: String -> Grid
getGrid string = grid
  where key = filter (not . isControl) string
        binaryHashes = map (\i -> knotHash Binary (key ++ "-" ++ show i)) [0..127]
        toRow = \binaryHash -> map (\char -> if char == '0' then Free else Used) binaryHash
        grid = map toRow binaryHashes

countState :: State -> Grid -> Count
countState state grid = 
  let countRow =  \row -> foldl (\count digit -> count + if digit == state then 1 else 0) 0 row
  in foldl (\count row -> count + countRow row) 0 grid
