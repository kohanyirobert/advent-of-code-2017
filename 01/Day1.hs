module Day1 where

toDigits :: Integer -> [Int]
toDigits number
  | number == 0 = []
  | otherwise = toDigits quotient ++ [remainder]
  where
    remainder = fromIntegral (number `mod` 10) :: Int
    quotient = number `div` 10

sumEqualDigitsN :: [Int] -> Int -> Int -> Int
sumEqualDigitsN digits n i
  | i == size = 0
  | curr == next = curr + sumEqualDigitsN digits n (i + 1)
  | otherwise = 0 + sumEqualDigitsN digits n (i + 1)
  where
    size = length digits
    curr = digits !! i
    next = digits !! ((i + n) `mod` size)
