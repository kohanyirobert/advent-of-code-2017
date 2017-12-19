module Day9 where

type Count = Int
type Depth = Int
type Total = Int

data State = Group Count Depth Total | Garbage Count Depth Total
  deriving Show

getChars :: String -> String
getChars string = head $ lines string

countGroups' :: State -> String -> State
countGroups' state [] = state
countGroups' state ('!' : char : chars) = countGroups' state chars
countGroups' state@(Garbage count depth total) (char : chars)
  | char == '>' = countGroups' (Group count depth total) chars
  | otherwise = countGroups' (Garbage count depth (total + 1)) chars
countGroups' state@(Group count depth total) (char : chars)
  | char == '{' = countGroups' (Group count (depth + 1) total) chars
  | char == '}' = countGroups' (Group (count + depth) (depth - 1) total) chars
  | char == ',' = countGroups' state chars
  | char == '<' = countGroups' (Garbage count depth total) chars
  | otherwise = state

countGroups :: String -> Int
countGroups string =
  let (Group count _ _) = countGroups' (Group 0 0 0) string
  in count

totalGarbage :: String -> Int
totalGarbage string =
  let (Group _ _ total) = countGroups' (Group 0 0 0) string
  in total
