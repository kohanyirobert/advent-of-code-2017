import Day24

import Data.Ord (comparing)
import Data.Monoid ((<>))

main = do
  string <- getContents
  print . maximum
        . map bridgeStrength
        . buildBridges (comparing length <> comparing bridgeStrength)
        . getMagnets
        $ string
