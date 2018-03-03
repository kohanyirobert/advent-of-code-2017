module Day24 where

import Data.List (maximumBy)

type Pins = Int

type Locked = Bool

type Bridge = [Magnet]

type Strength = Int

type Heuristic = Bridge -> Bridge -> Ordering

data Port =
  Port Pins
       Locked
  deriving (Ord, Eq)

instance Show Port where
  show (Port pins locked) =
    show pins ++
    if locked
      then "*"
      else ""

data Magnet =
  Magnet Port
         Port
  deriving (Ord, Eq)

instance Show Magnet where
  show (Magnet a b) = (show a) ++ "/" ++ (show b)

slashToSpace :: Char -> Char
slashToSpace c =
  if c == '/'
    then ' '
    else c

stringToMagnet :: String -> Magnet
stringToMagnet string =
  let leftAndRightPins = map read . words . map slashToSpace $ string
      leftPort = Port (leftAndRightPins !! 0) False
      rightPort = Port (leftAndRightPins !! 1) False
  in Magnet leftPort rightPort

getMagnets :: String -> [Magnet]
getMagnets = map stringToMagnet . lines

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

isRootMagnet :: Magnet -> Bool
isRootMagnet (Magnet a b) = isRootPort a || isRootPort b

getRootMagnets :: [Magnet] -> [Magnet]
getRootMagnets = map toRootMagnet . filter isRootMagnet

sameMagnets :: Magnet -> Magnet -> Bool
sameMagnets (Magnet (Port a _) (Port b _)) (Magnet (Port c _) (Port d _)) =
  a == c && b == d

differentMagnets :: Magnet -> Magnet -> Bool
differentMagnets a b = not (sameMagnets a b)

isCompatiblePorts :: Port -> Port -> Bool
isCompatiblePorts (Port a False) (Port b False) = a == b
isCompatiblePorts _ _ = False

isCompatibleMagnets :: Magnet -> Magnet -> Bool
isCompatibleMagnets p@(Magnet a b) q@(Magnet c d)
  | sameMagnets p q = error "This shouldn't happen"
  | otherwise =
    isCompatiblePorts a c ||
    isCompatiblePorts a d || isCompatiblePorts b c || isCompatiblePorts b d

connectMagnet :: Magnet -> Magnet -> Magnet
connectMagnet (Magnet p@(Port a a') q@(Port b b')) (Magnet r@(Port c c') s@(Port d d'))
  | isCompatiblePorts p r || isCompatiblePorts q r = Magnet (Port c True) s
  | isCompatiblePorts p s || isCompatiblePorts q s = Magnet r (Port d True)
  | otherwise = error "This shouldn't happen"

portStrength :: Port -> Strength
portStrength (Port pins _) = pins

magnetStrength :: Magnet -> Strength
magnetStrength (Magnet a b) = portStrength a + portStrength b

bridgeStrength :: Bridge -> Strength
bridgeStrength = foldl (\a b -> a + magnetStrength b) 0

buildBridge :: Heuristic -> [Magnet] -> Magnet -> Bridge
buildBridge heuristic ms m
  | null candidates = [m]
  | otherwise =
    let bridges =
          map
            (\m' ->
               let ms' = filter (differentMagnets m') ms
               in buildBridge heuristic ms' m')
            candidates
    in m : maximumBy heuristic bridges
  where
    candidates = map (connectMagnet m) . filter (isCompatibleMagnets m) $ ms

buildBridges :: Heuristic -> [Magnet] -> [Bridge]
buildBridges heuristic ms =
  map (\rm -> buildBridge heuristic (filter (differentMagnets rm) ms) rm) rms
  where
    rms = getRootMagnets ms
