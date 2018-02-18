import Day24

import Data.Ord (comparing)
import Data.Function (on)

main = do
  string <- getContents
  print . maximum
        . map bridgeStrength
        . buildBridges (comparing bridgeStrength)
        . getMagnets
        $ string
