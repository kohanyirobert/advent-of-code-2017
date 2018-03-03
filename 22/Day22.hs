module Day22 where

import qualified Data.Map.Strict as Map

import Data.List (foldl')

type Count = Int

type Coordinate = (Int, Int)

type Node = Char

type Cluster = Map.Map Coordinate Node

type Step = (Node, Direction, Count)

type Behavior = Step -> Step

data Direction
  = Upward
  | Downward
  | Leftward
  | Rightward
  deriving (Show)

data Carrier = Carrier
  { coordinate :: Coordinate
  , direction :: Direction
  , infects :: !Count
  } deriving (Show)

data State = State
  { cluster :: Cluster
  , carrier :: Carrier
  } deriving (Show)

getCluster :: String -> Cluster
getCluster string =
  let rows = lines string
      width = length rows
      halfWidth = width `div` 2
      indexedRows = zip [(0 - halfWidth) ..] rows
      foldRow cluster (j, row) =
        let cells = row
            height = length rows
            halfHeight = height `div` 2
            indexedCells =
              map (\(i, cell) -> ((i, j), cell)) $
              zip [(0 - halfHeight) ..] cells
        in foldl foldCell cluster indexedCells
      foldCell cluster (coordinate, cell) = Map.insert coordinate cell cluster
  in foldl foldRow Map.empty indexedRows

turnRight :: Direction -> Direction
turnRight Upward = Rightward
turnRight Rightward = Downward
turnRight Downward = Leftward
turnRight Leftward = Upward

turnLeft :: Direction -> Direction
turnLeft Upward = Leftward
turnLeft Leftward = Downward
turnLeft Downward = Rightward
turnLeft Rightward = Upward

turnBack :: Direction -> Direction
turnBack Upward = Downward
turnBack Downward = Upward
turnBack Leftward = Rightward
turnBack Rightward = Leftward

moveForward :: Coordinate -> Direction -> Coordinate
moveForward (i, j) Upward = (i, j - 1)
moveForward (i, j) Downward = (i, j + 1)
moveForward (i, j) Leftward = (i - 1, j)
moveForward (i, j) Rightward = (i + 1, j)

clean :: Node
clean = '.'

infected :: Node
infected = '#'

weakened :: Node
weakened = 'W'

flagged :: Node
flagged = 'F'

startCarrier :: Carrier
startCarrier = Carrier (0, 0) Upward 0

defaultBehavior :: Behavior
defaultBehavior ('.', direction, infects) =
  (infected, turnLeft direction, infects + 1)
defaultBehavior ('#', direction, infects) =
  (clean, turnRight direction, infects)

evolvedBehavior :: Behavior
evolvedBehavior ('.', direction, infects) =
  (weakened, turnLeft direction, infects)
evolvedBehavior ('W', direction, infects) = (infected, direction, infects + 1)
evolvedBehavior ('#', direction, infects) =
  (flagged, turnRight direction, infects)
evolvedBehavior ('F', direction, infects) = (clean, turnBack direction, infects)

burst :: Behavior -> State -> State
burst behavior (State cluster (Carrier coordinate direction infects)) =
  let node = Map.findWithDefault clean coordinate cluster
      (node', direction', infects') = behavior (node, direction, infects)
      cluster' = Map.insert coordinate node' cluster
      coordinate' = moveForward coordinate direction'
  in (State cluster' (Carrier coordinate' direction' infects'))

bursts :: Count -> Behavior -> State -> State
bursts count behavior state =
  foldl' (\s _ -> burst behavior s) state [1 .. count]
