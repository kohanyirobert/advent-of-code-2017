module Day1 where

toDigits :: Integer -> [Int]
toDigits n
  | n == 0 = []
  | otherwise = (toDigits d) ++ [r]
  where
    r = fromIntegral (n `mod` 10) :: Int
    d = n `div` 10

sumEqualDigitsN :: [Int] -> Int -> Int -> Int
sumEqualDigitsN xs n i
  | i == l = 0
  | a == b = a + (sumEqualDigitsN xs n (i + 1))
  | otherwise = 0 + (sumEqualDigitsN xs n (i + 1))
  where l = length xs
        a = xs !! i
        b = xs !! ((i + n) `mod` l)
