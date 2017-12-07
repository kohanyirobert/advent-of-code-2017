module Day7 where

import Data.Char (isAlphaNum, isSpace, isControl)
import Data.List (partition)

data Detail = Detail String Int
  deriving Show

data Tower = Tower Detail [String]
  deriving Show

type Towers = [Tower]
type Nodes = [Node]

getTowers :: String -> Towers
getTowers string = map toTower dataRecords
  where toDataLine = \x -> filter (\y -> isAlphaNum y || isSpace y && not (isControl y)) x
        dataLines = map toDataLine $ lines string
        dataRecords = map words dataLines
        toTower = \(name : weight : children) -> Tower (Detail name (read weight)) children

isRoot :: Tower -> Towers -> Bool
isRoot (Tower (Detail name _) _) towers = all (\(Tower _ children) -> not (name `elem` children)) towers

findRoot :: Towers -> Tower
findRoot towers = head $ filter (\x -> isRoot x towers) towers

partitionTowers :: Towers -> (Towers, Towers)
partitionTowers towers = (nodes, leafs)
  where (leafs, nodes) = partition (\(Tower _ children) -> children == []) towers

findParent :: Tower -> Nodes -> Maybe Node
findParent _ [] = Nothing
findParent tower@(Tower (Detail target _) _) (node@(Node (Detail name _) _) : nodes)
  | target == name = Just node
  | otherwise = findParent tower nodes
