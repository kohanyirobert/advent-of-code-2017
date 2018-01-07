import Day20

import Data.List (elemIndex)
import Data.Maybe (fromJust)

main = do
  string <- getContents
  let particles = getParticles string
      closest = closestParticle particles
  print $ fromJust $ closest `elemIndex` particles
