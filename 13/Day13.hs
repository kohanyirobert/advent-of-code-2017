module Day13 where

import Data.Char (isDigit, isSpace)

type Depth = Int

type Range = Int

type Severity = Int

type Picosecond = Int

type Position = Int

data Direction
  = Up
  | Down
  deriving (Eq, Show)

data Scanner
  = None
  | Scanner Position
            Direction
  deriving (Eq, Show)

data Layer =
  Layer Depth
        Range
        Severity
        Scanner
  deriving (Eq, Show)

fillMissingLayers :: [Layer] -> [Layer]
fillMissingLayers [] = []
fillMissingLayers (layer:[]) = [layer]
fillMissingLayers (layer@(Layer depth _ _ _):layers@(Layer nextDepth _ _ _:_))
  | depth + 1 == nextDepth = layer : fillMissingLayers layers
  | otherwise = layer : fillMissingLayers (missingLayer : layers)
  where
    missingLayer = Layer (depth + 1) 0 0 None

getLayers :: String -> [Layer]
getLayers string = allLayers
  where
    keepNumbers = \line -> filter (\char -> isDigit char || isSpace char) line
    dataLines = map keepNumbers $ lines string
    toLayer =
      \(d:r:[]) ->
        let depth = read d
            range = read r
            severity = depth * range
        in Layer depth range severity (Scanner 0 Down)
    layers = map toLayer $ map words dataLines
    allLayers = fillMissingLayers layers

moveScanner :: Range -> Scanner -> Scanner
moveScanner _ None = None
moveScanner range (Scanner position Down)
  | position == range - 1 = Scanner (position - 1) Up
  | otherwise = Scanner (position + 1) Down
moveScanner range (Scanner position Up)
  | position == 0 = Scanner (position + 1) Down
  | otherwise = Scanner (position - 1) Up

updateLayers :: [Layer] -> [Layer]
updateLayers layers = map updateLayer layers
  where
    updateLayer =
      \(Layer depth range severity scanner) ->
        Layer depth range severity (moveScanner range scanner)

ridePacket' :: [Layer] -> [Layer] -> [Layer]
ridePacket' [] triggeredLayers = triggeredLayers
ridePacket' (layer:layers) triggeredLayers =
  ridePacket' nextLayers nextTriggeredLayers
  where
    nextTriggeredLayers =
      case layer of
        (Layer _ _ _ (Scanner 0 _)) -> layer : triggeredLayers
        _ -> triggeredLayers
    nextLayers = updateLayers layers

ridePacket :: [Layer] -> [Layer]
ridePacket layers = ridePacket' layers []

slipPacket' :: Picosecond -> [Layer] -> Picosecond
slipPacket' delay layers
  | triggeredLayers == [] = delay
  | otherwise = slipPacket' nextDelay nextLayers
  where
    triggeredLayers = ridePacket layers
    nextDelay = delay + 1
    nextLayers = updateLayers layers

slipPacket :: [Layer] -> Picosecond
slipPacket layers = slipPacket' 0 layers
