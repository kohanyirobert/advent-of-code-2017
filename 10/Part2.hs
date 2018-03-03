import Day10

main = do
  string <- getContents
  putStrLn $ knotHash Hexadecimal $ string
