import Day24

import Data.Monoid ((<>))
import Data.Ord (comparing)

main = do
  string <- getContents
  print .
    maximum .
    map bridgeStrength .
    buildBridges (comparing length <> comparing bridgeStrength) . getMagnets $
    string
