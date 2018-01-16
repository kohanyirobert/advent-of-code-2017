module Day21 where

import qualified Data.Map.Strict as Map

import Data.List (transpose, nub)

type Pattern = [String]
type Rule = (Pattern, Pattern)
type Rules = Map.Map Pattern Pattern
type Coordinate = (Int, Int)
type Grid = Map.Map Coordinate Pattern

slashToSpace :: Char -> Char
slashToSpace c = if c == '/'
                 then ' '
                 else c

startPattern :: Pattern
startPattern = [".#."
               ,"..#"
               ,"###"
               ]

stringToRule :: String -> Rule
stringToRule string
  = let (input : output : []) = map words
                              . map (map slashToSpace)
                              . filter (/= "=>")
                              . words
                              $ string
    in (input, output)

updateRules :: [Rule] -> [Rule]
updateRules [] = []
updateRules ((input, o) : rules)
  = let ruleVariations = map (\i -> (i, o)) . patternVariations $ input
    in ruleVariations ++ updateRules rules

getRules :: String -> Rules
getRules = Map.fromList . updateRules . map stringToRule . lines

patternVariations :: Pattern -> [Pattern]
patternVariations pattern
  = let a = pattern
        b = transpose a
        c = reverse b
        d = transpose c
        e = reverse d
        f = transpose e
        g = reverse f
        h = transpose g
    in nub [a, b, c, d, e, f, g, h]

quadrantInPatternAt :: Pattern -> Int -> Coordinate -> Pattern
quadrantInPatternAt pattern size (i, j)
  = let range = [0..size - 1]
    in map (\p -> map (\q -> (pattern !! (size * j + q)) !! (size * i + p)) range) range

patternToGrid :: Pattern -> Grid
patternToGrid pattern
  = let patternSize = length pattern
        size = if patternSize `mod` 2 == 0
               then 2
               else 3
        gridSize = patternSize `div` size
        range = [0..gridSize - 1]
        coordinates = concat
                    . map (\i -> map (\j -> (i, j)) range)
                    $ range
    in foldl (\grid coordinate -> Map.insert coordinate (quadrantInPatternAt pattern size coordinate) grid) Map.empty coordinates

charInGridAt :: Grid -> Coordinate -> Char
charInGridAt grid (i, j)
  = let size = length . head . Map.elems $ grid
        x = i `div` size
        y = j `div` size
        (Just quadrant) = Map.lookup (x, y) grid
        p = i `mod` size
        q = j `mod` size
    in (quadrant !! q) !! p

rowInGridAt :: Grid -> String -> Coordinate -> String
rowInGridAt grid string coordinate = string ++ [charInGridAt grid coordinate]

gridToPattern :: Grid -> Pattern
gridToPattern grid
  = let gridSize = (1+) . fst . maximum . Map.keys $ grid
        subPatternSize = length . head . Map.elems $ grid
        newPatternSize = gridSize * subPatternSize
        range = [0..newPatternSize - 1]
        coordinateGroups = map (\i -> map (\j -> (i, j)) range) range
    in foldl (\pattern coordinates -> pattern ++ [foldl (rowInGridAt grid) "" coordinates]) [] coordinateGroups

enhancePattern :: Rules -> Pattern -> Pattern
enhancePattern rules pattern
  | size `elem` [2, 3] = let (Just pattern') = Map.lookup pattern rules in pattern'
  | otherwise = gridToPattern
              . Map.map (enhancePattern rules)
              . patternToGrid
              $ pattern
  where size = length pattern

patternToString :: Pattern -> String
patternToString pattern = unlines pattern

countCharInPattern :: Char -> Pattern -> Int
countCharInPattern char pattern
  = length
  . filter (== char)
  . patternToString
  $ pattern
