module Day11 where

type HexCoordinate = (Int, Int, Int)
type HexDistance = Int
type HexDirection = HexCoordinate -> HexCoordinate

toNorth (x, y, z) = (x, y + 1, z - 1)
toSouth (x, y, z) = (x, y - 1, z + 1)

toNorthEast (x, y, z) = (x + 1, y, z - 1)
toSouthWest (x, y, z) = (x - 1, y, z + 1)

toNorthWest (x, y, z) = (x - 1, y + 1, z)
toSouthEast (x, y, z) = (x + 1, y - 1, z)

hexDistance :: HexCoordinate -> HexCoordinate -> HexDistance
hexDistance (x0, y0, z0) (x1, y1, z1) = (sum $ map abs [x0 - x1, y0 - y1, z0 - z1]) `div` 2

hexTransform :: HexCoordinate -> [HexDirection] -> HexCoordinate
hexTransform coordinate directions = foldl (\c d -> d c) coordinate directions

getDirections :: String -> [HexDirection]
getDirections string = map stringToDirection $ words $ map commaToSpace string
  where commaToSpace = \c -> if c == ',' then ' ' else c
        stringToDirection = \s -> if s == "n"
                                  then toNorth
                                  else if s == "s"
                                       then toSouth
                                       else if s == "ne"
                                            then toNorthEast
                                            else if s == "sw"
                                                 then toSouthWest
                                                 else if s == "nw"
                                                      then toNorthWest
                                                      else toSouthEast
