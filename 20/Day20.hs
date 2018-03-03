module Day20 where

import Data.Char (isDigit)

type Vector = (Int, Int, Int)

data Particle = Particle
  { position :: Vector
  , velocity :: Vector
  , acceleration :: Vector
  } deriving (Show)

instance Eq Particle where
  (Particle p0 _ _) == (Particle p1 _ _) = p0 == p1

instance Ord Particle where
  a `compare` b = origoDistance a `compare` origoDistance b

toVector :: String -> Vector
toVector string =
  let xyz =
        map read .
        words .
        map
          (\c ->
             if c == ','
               then ' '
               else c) .
        filter (\c -> isDigit c || c == '-' || c == ',') $
        string
  in (xyz !! 0, xyz !! 1, xyz !! 2)

addVector :: Vector -> Vector -> Vector
addVector (x0, y0, z0) (x1, y1, z1) = (x0 + x1, y0 + y1, z0 + z1)

toParticle :: [String] -> Particle
toParticle (p:v:a:[]) = Particle (toVector p) (toVector v) (toVector a)

getParticles string = map toParticle . map words . lines $ string

origoDistance :: Particle -> Int
origoDistance (Particle _ _ (x, y, z)) = abs x + abs y + abs z

closestParticle :: [Particle] -> Particle
closestParticle particles = minimum particles

tickParticle :: Particle -> Particle
tickParticle (Particle p v a) =
  let v' = addVector v a
      p' = addVector p v'
  in Particle p' v' a

countCollisions :: [Particle] -> Particle -> Int
countCollisions [] _ = 0
countCollisions (particle:particles) p =
  countCollisions particles p +
  if particle == p
    then 1
    else 0

tickParticles :: [Particle] -> [Particle]
tickParticles particles =
  map tickParticle . filter ((<= 1) . (countCollisions particles)) $ particles
