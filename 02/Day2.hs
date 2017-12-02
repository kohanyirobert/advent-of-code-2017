module Day2 where

getTable :: String -> [[Int]]
getTable string = table
  where cells = lines string
        rows = map words cells
        table = map (map read) rows

getChecksum :: [[Int]] -> ([Int] -> Int) -> Int
getChecksum table rowMapper = sum (map rowMapper table)
