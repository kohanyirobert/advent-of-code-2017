import Day20

main = do
  string <- getContents
  let particles = getParticles string
      particles' = (iterate tickParticles particles) !! 100
  print $ length particles'
