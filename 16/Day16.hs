module Day16 where

import Data.Char (isControl)

type Count = Int

type Position = Int

type Program = Char

data DanceMove
  = Spin Count
  | Exchange Position
             Position
  | Partner Program
            Program
  deriving (Show)

charToSpace :: Char -> Char -> Char
charToSpace target char =
  if char == target
    then ' '
    else char

stringToDanceMove :: String -> DanceMove
stringToDanceMove (c:string)
  | c == 's' = Spin (read string)
  | c == 'x' = Exchange (read a) (read b)
  | c == 'p' = Partner (head a) (head b)
  where
    a:b:_ = words . map (charToSpace '/') $ string

getDanceMoves :: String -> [DanceMove]
getDanceMoves string =
  map stringToDanceMove .
  words . map (charToSpace ',') . filter (not . isControl) $
  string

programIndex :: Program -> [Program] -> Position
programIndex program (p:ps)
  | p == program = 0
  | otherwise = 1 + programIndex program ps

programSwap :: Position -> Position -> [Program] -> [Program]
programSwap a b programs
  | a == b = programs
  | otherwise =
    let size = length programs
        start = max 0 $ min a b
        end = min (size - 1) $ max a b
        (partA, tempA) = splitAt start programs
        (programA, tempB) = splitAt 1 tempA
        (partB, tempC) = splitAt (end - start - 1) tempB
        (programB, partC) = splitAt 1 tempC
    in partA ++ programB ++ partB ++ programA ++ partC

applyDanceMove :: [Program] -> DanceMove -> [Program]
applyDanceMove programs (Spin x) =
  let size = length programs
      (firstPart, secondPart) = splitAt (size - x) programs
  in secondPart ++ firstPart
applyDanceMove programs (Partner a b) =
  let positionA = programIndex a programs
      positionB = programIndex b programs
  in applyDanceMove programs (Exchange positionA positionB)
applyDanceMove programs (Exchange a b) = programSwap a b programs

followDanceMoves :: [Program] -> [DanceMove] -> [Program]
followDanceMoves programs moves = foldl applyDanceMove programs moves

findProgramVariations' :: [[Program]] -> [Program] -> [DanceMove] -> [[Program]]
findProgramVariations' variations programs moves
  | programs `elem` variations = variations
  | otherwise = findProgramVariations' variations' programs' moves
  where
    programs' = followDanceMoves programs moves
    variations' = variations ++ [programs]

findProgramVariations :: [Program] -> [DanceMove] -> [[Program]]
findProgramVariations = findProgramVariations' []
