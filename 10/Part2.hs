import Day10

main = do
  string <- getContents
  let sizes = getByteSizes string ++ [17, 31, 73, 47, 23]
  print $ hashToHex $ denseHash 16 $ sparseHash 64 sizes [0..255]
