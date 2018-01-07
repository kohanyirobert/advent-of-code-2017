module Day20 where

import Data.Char (isDigit)

type Vector = (Int, Int, Int)

data Particle = Particle
  { position :: Vector
  , velocity :: Vector
  , acceleration :: Vector
  } deriving (Eq, Show)

instance Ord Particle where
  a `compare` b = origoDistance a `compare` origoDistance b

toVector :: String -> Vector
toVector string
  = let xyz = map read
            . words
            . map (\c -> if c == ',' then ' ' else c)
            . filter (\c -> isDigit c || c == '-' || c == ',')
            $ string
    in (xyz !! 0, xyz !! 1, xyz !! 2)

toParticle :: [String] -> Particle
toParticle (p : v : a : []) = Particle (toVector p) (toVector v) (toVector a)

getParticles string
  = map toParticle
  . map words
  . lines
  $ string

origoDistance :: Particle -> Int
origoDistance (Particle _ _ (x, y, z)) = abs x + abs y + abs z

closestParticle :: [Particle] -> Particle
closestParticle particles = minimum particles
