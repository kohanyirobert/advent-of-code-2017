module Day24 where

import Data.Maybe (fromJust)
import Data.List (find)

type Pins = Int
type Locked = Bool
type Bridge = [Magnet]
type Strength = Int

data Port = Port Pins Locked
  deriving (Ord, Eq)

instance Show Port where
  show (Port pins locked) = show pins ++ if locked then "*" else ""

data Magnet = Magnet Port Port
  deriving (Ord, Eq)

instance Show Magnet where
  show (Magnet a b) = (show a) ++ "/" ++ (show b)

data Tree = Tree Magnet [Tree]
  deriving (Eq)

instance Show Tree where
  show (Tree p ts) = show p ++ " " ++ show ts

slashToSpace :: Char -> Char
slashToSpace c = if c == '/' then ' ' else c

stringToMagnet :: String -> Magnet
stringToMagnet string
  = let leftAndRightPins = map read
                         . words
                         . map slashToSpace
                         $ string
        leftPort = Port (leftAndRightPins !! 0) False
        rightPort = Port (leftAndRightPins !! 1) False
    in Magnet leftPort rightPort

getMagnets :: String -> [Magnet]
getMagnets
  = map stringToMagnet
  . lines

isRootPort :: Port -> Bool
isRootPort (Port 0 _) = True
isRootPort _ = False

lockPort :: Port -> Port
lockPort (Port pins False) = Port pins True
lockPort (Port _ True) = error "This shouldn't happen"

toRootMagnet :: Magnet -> Magnet
toRootMagnet (Magnet a b)
  | isRootPort a = Magnet (lockPort a) b
  | isRootPort b = Magnet a (lockPort b)
  | otherwise = error "This shouldn't happen"

isLockedPort :: Port -> Bool
isLockedPort (Port _ locked) = locked

isUnlockedPort :: Port -> Bool
isUnlockedPort = not . isLockedPort

portToStrength :: Port -> Strength
portToStrength (Port pins _) = pins

isRootMagnet :: Magnet -> Bool
isRootMagnet (Magnet a b) = isRootPort a || isRootPort b

findRootMagnets :: [Magnet] -> [Magnet]
findRootMagnets = filter isRootMagnet

sameMagnets :: Magnet -> Magnet -> Bool
sameMagnets (Magnet (Port a _) (Port b _))
            (Magnet (Port c _) (Port d _)) = a == c && b == d

differentMagnets :: Magnet -> Magnet -> Bool
differentMagnets a b = not (sameMagnets a b)

isCompatiblePorts :: Port -> Port -> Bool
isCompatiblePorts (Port a False) (Port b False) = a == b
isCompatiblePorts _ _ = False

isCompatibleMagnets :: Magnet -> Magnet -> Bool
isCompatibleMagnets (Magnet a b) (Magnet c d)
  = isCompatiblePorts a c
  || isCompatiblePorts a d
  || isCompatiblePorts b c
  || isCompatiblePorts b d

connectMagnet :: Magnet -> Magnet -> Magnet
connectMagnet (Magnet p@(Port a a') q@(Port b b'))
              (Magnet r@(Port c c') s@(Port d d'))
  | isCompatiblePorts p r || isCompatiblePorts q r = let r' = Port c True
                                                     in Magnet r' s
  | isCompatiblePorts p s || isCompatiblePorts q s = let s' = Port d True
                                                     in Magnet r s'
  | otherwise = error "This shouldn't happen"

magnetToStrength :: Magnet -> Strength
magnetToStrength (Magnet a b) = portToStrength a + portToStrength b

nextMagnet :: [Magnet] -> Tree -> Maybe Magnet
nextMagnet [] _ = Nothing
nextMagnet (m : ms) t
  = let t' = updateTree m t
    in if t == t'
       then nextMagnet ms t
       else Just m

updateTree :: Magnet -> Tree -> Tree
updateTree b t@(Tree a ts)
  = let sameThing = sameMagnets a b
        alreadyInserted = Nothing /= find (\(Tree a' _) -> sameMagnets a' b) ts
        ts' = map (\subT@(Tree a' _) -> if sameMagnets a' b
                                        then subT
                                        else updateTree b subT) ts
    in if sameThing || alreadyInserted
       then Tree a ts'
       else if isCompatibleMagnets a b
            then let b' = connectMagnet a b
                 in Tree a (Tree b' [] : ts')
            else Tree a ts'

buildTree :: [Magnet] -> Tree -> Tree
buildTree ms t
  = let m = nextMagnet ms t
    in if m == Nothing
       then t
       else let m' = fromJust m
                t' = updateTree m' t
                ms' = filter (/= m') ms
                ms'' = ms' ++ [m']
            in buildTree ms'' t'

buildTrees :: [Magnet] -> [Tree]
buildTrees magnets
  = let rootMagnets = map toRootMagnet . findRootMagnets $ magnets
        getOtherMagnets rootMagnet = filter (differentMagnets rootMagnet) magnets
    in map (\rootMagnet -> buildTree (getOtherMagnets rootMagnet) (Tree rootMagnet [])) rootMagnets

findBridges :: Tree -> [Bridge]
findBridges (Tree a []) = [[a]]
findBridges (Tree a ts) = map (a :)  (foldr (\t bs -> bs ++ findBridges t) [] ts)

bridgeToStrength :: Bridge -> Strength
bridgeToStrength = foldl (\a b -> a + magnetToStrength b) 0
