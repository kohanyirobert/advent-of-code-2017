module Day7 where

import Data.Char (isAlphaNum, isControl, isSpace)
import Data.List (partition)
import Data.Maybe (fromJust, isJust)

data Detail =
  Detail String
         Int
         Int
  deriving (Eq, Show)

data Tower =
  Tower Detail
        [String]
  deriving (Eq, Show)

data Node =
  Node Detail
       [Node]
  deriving (Eq, Show)

type Towers = [Tower]

type Nodes = [Node]

getTowers :: String -> Towers
getTowers string = map toTower dataRecords
  where
    toDataLine =
      \x -> filter (\y -> isAlphaNum y || isSpace y && not (isControl y)) x
    dataLines = map toDataLine $ lines string
    dataRecords = map words dataLines
    toTower =
      \(name:weight:children) -> Tower (Detail name (read weight) 0) children

isRoot :: Tower -> Towers -> Bool
isRoot (Tower (Detail name _ _) _) towers =
  all (\(Tower _ children) -> not (name `elem` children)) towers

findRoot :: Towers -> Tower
findRoot towers = head $ filter (\x -> isRoot x towers) towers

partitionTowers :: Towers -> (Towers, Towers)
partitionTowers towers = (nodes, leafs)
  where
    (leafs, nodes) = partition (\(Tower _ children) -> children == []) towers

findTower :: String -> Towers -> Tower
findTower target (tower@(Tower (Detail name _ _) _):towers)
  | target == name = tower
  | otherwise = findTower target towers

toNode :: Tower -> Towers -> Node
toNode (Tower detail children) towers = Node detail nodes
  where
    nodes = map (\x -> toNode (findTower x towers) towers) children

findNode :: String -> Node -> Maybe Node
findNode target node@(Node (Detail name _ _) children)
  | target == name = Just node
  | otherwise =
    let matches = filter isJust $ map (\x -> findNode target x) children
    in if matches == []
         then Nothing
         else head matches

getWeight :: Node -> Int
getWeight node@(Node (Detail _ weight _) children) =
  weight + sum (map getWeight children)

updateTreeSubWeights :: Node -> Node
updateTreeSubWeights node@(Node (Detail name weight _) children) = newNode
  where
    newChildren = map updateTreeSubWeights children
    newNode = Node (Detail name weight (getWeight node)) newChildren

buildTree :: Towers -> Node
buildTree towers = finalTree
  where
    (nodes, _) = partitionTowers towers
    root = findRoot nodes
    tree = toNode root towers
    finalTree = updateTreeSubWeights tree

isBalanced :: Node -> Bool
isBalanced node@(Node (Detail _ _ total) children)
  | size < 2 = True
  | otherwise = (minSize == maxSize) || (minSize == 0 || maxSize == 0)
  where
    size = length children
    weights = map getWeight children
    minWeight = minimum weights
    maxWeight = maximum weights
    minElems = filter (== minWeight) weights
    maxElems = filter (== maxWeight) weights
    minSize = length minElems
    maxSize = length maxElems

findUnbalanced :: Node -> Maybe Node
findUnbalanced node@(Node _ []) = Nothing
findUnbalanced node@(Node _ children)
  | isBalanced node = Nothing
  | otherwise =
    if matches == []
      then Just node
      else head matches
  where
    matches = filter isJust $ map findUnbalanced children

getBalancedWeight :: Node -> Int
getBalancedWeight (Node (Detail _ _ _) children) = targetWeight + weightDiff
  where
    weights = map getWeight children
    minWeight = minimum weights
    maxWeight = maximum weights
    weightDiff = minWeight - maxWeight
    minElems = filter (== minWeight) weights
    maxElems = filter (== maxWeight) weights
    minSize = length minElems
    maxSize = length maxElems
    targetTotal =
      if minSize < maxSize
        then minWeight
        else maxWeight
    (Node (Detail _ targetWeight _) _) =
      head $
      filter (\(Node (Detail _ _ total) _) -> total == targetTotal) children
