module Day25 where

import qualified Data.Map.Strict as Map

import Data.Char (isDigit)
import Data.List (isInfixOf, foldl')

type State = Char
type Tape = Map.Map Int Int
type Position = Int
type Count = Int
type Value = Int
type Steps = Map.Map State Step
type Step = Machine -> Machine

data Direction = Leftward | Rightward
  deriving (Eq, Show)

data Machine = Machine
  { tape :: Tape
  , cursor :: Position
  , state :: State
  } deriving (Show)

data Blueprint = Blueprint
  { startState :: State
  , checksumAfter :: Count
  , steps :: Steps
  }

getParts :: [String] -> [[String]]
getParts [] = []
getParts strings
  = let (a, b) = break (== "") strings
        c = drop 1 b
    in a : getParts c

getStartState :: String -> State
getStartState string = string !! index
  where index = (length string) - 2

getChecksumAfter :: String -> Count
getChecksumAfter = read
                 . filter isDigit

getState :: String -> State
getState string = string !! index
  where index = (length string) - 2

getValue :: String -> Value
getValue = read
         . filter isDigit

getDirection :: String -> Direction
getDirection string
  | isInfixOf "left" string = Leftward
  | otherwise = Rightward

toStep :: [String] -> Step
toStep strings
  = let (first, second) = splitAt (length strings `div` 2) strings
    in \machine -> let c = cursor machine
                       t = tape machine
                       t' = if Map.member c t
                            then t
                            else Map.insert c 0 t
                       (Just v) = Map.lookup c t'
                       branch = if v == getValue (first !! 0)
                                then first
                                else if v == getValue (second !! 0)
                                     then second
                                     else error "This shouldn't happen"
                       v' = getValue (branch !! 1)
                       direction = getDirection (branch !! 2)
                       t'' = Map.insert c v' t'
                       c' = if direction == Leftward
                            then c - 1
                            else c + 1
                       s' = getState (branch !! 3)
                   in machine { tape = t''
                              , cursor = c'
                              , state = s'
                              }

toStateAndStep :: [String] -> (State, Step)
toStateAndStep strings
  = let currentState = getState (strings !! 0)
    in (currentState, toStep (drop 1 strings))

getSteps :: [[String]] -> Steps
getSteps = Map.fromList
         . map toStateAndStep

getBlueprint :: String -> Blueprint
getBlueprint string
  = let parts = getParts
              . lines
              $ string
        detailsPart = parts !! 0
        stepsPart = drop 1 parts
        startState = getStartState (detailsPart !! 0)
        checksumAfter = getChecksumAfter (detailsPart !! 1)
        steps = getSteps stepsPart
    in Blueprint { startState = startState
                 , checksumAfter = checksumAfter
                 , steps = steps
                 }

next :: Blueprint -> Machine -> Machine
next (Blueprint {steps = ss}) m@(Machine {state = s})
  = let (Just step) = Map.lookup s ss
        m' = step m
    in m'

run :: Blueprint -> Machine
run bp@(Blueprint {startState = s, checksumAfter = c})
  = let m = Machine { tape = Map.singleton 0 0
                    , cursor = 0
                    , state = s
                    }
    in foldl' (\m' _ -> next bp m') m [1..c]

checksum :: Machine -> Int
checksum (Machine {tape = t}) = Map.foldl (+) 0 t
