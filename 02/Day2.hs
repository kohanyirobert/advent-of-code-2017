module Day2 where

getTable :: String -> [[Int]]
getTable string = table
  where cells = lines string
        rows = map words cells
        table = map (map read) rows

getChecksum :: [[Int]] -> Int
getChecksum table = sum (map calculate table)
  where calculate :: [Int] -> Int
        calculate row = abs (minimum row - maximum row)
