module Day2 where

getTable :: String -> [[Int]]
getTable string = map (map read) $ map words $ lines string

getChecksum :: [[Int]] -> ([Int] -> Int) -> Int
getChecksum table rowMapper = sum (map rowMapper table)
