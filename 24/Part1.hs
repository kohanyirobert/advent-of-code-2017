import Day24

import Data.Function (on)
import Data.Ord (comparing)

main = do
  string <- getContents
  print .
    maximum .
    map bridgeStrength . buildBridges (comparing bridgeStrength) . getMagnets $
    string
