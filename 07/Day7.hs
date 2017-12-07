module Day7 where

import Data.Char (isAlphaNum, isSpace, isControl)
import Data.List (partition)

data Detail = Detail String Int
  deriving Show

type Tower = (Detail, [String])
type Towers = [Tower]

getTowers :: String -> Towers
getTowers string = map toTower dataRecords
  where toDataLine = \x -> filter (\y -> isAlphaNum y || isSpace y && not (isControl y)) x
        dataLines = map toDataLine $ lines string
        dataRecords = map words dataLines
        toTower = \(name : weight : children) -> (Detail name (read weight), children)

isRoot :: Tower -> Towers -> Bool
isRoot (Detail name _, _) towers = all (\(_, children) -> not (name `elem` children)) towers

findRoot :: Towers -> Tower
findRoot towers = head $ filter (\x -> isRoot x towers) towers

partitionTowers :: Towers -> (Towers, Towers)
partitionTowers towers = (nodes, leafs)
  where (leafs, nodes) = partition (\x@(_, children) -> children == []) towers
