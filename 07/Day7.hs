module Day7 where

import Data.Char

type Tower = (String, Int, [String])
type Towers = [Tower]

getTowers :: String -> Towers
getTowers string = map toTower dataRecords
  where toDataLine = \x -> filter (\y -> isAlphaNum y || isSpace y && not (isControl y)) x
        dataLines = map toDataLine $ lines string
        dataRecords = map words dataLines
        toTower = \(name : weight : children) -> (name, read weight, children)

findPossibleRoots :: Towers -> Towers
findPossibleRoots towers = filter (\x@(_, _, children) -> children /= []) towers

isRoot :: Tower -> Towers -> Bool
isRoot (name, _, _) towers = all (\(_, _, children) -> not (name `elem` children)) towers

findRoot :: Towers -> Tower
findRoot towers = head $ filter (\x -> isRoot x towers) towers
