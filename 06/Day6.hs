module Day6 where

import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Data.Maybe

type Cycle = Int

type Position = Int

type Count = Int

type Banks = Seq.Seq Int

type States = Set.Set Banks

getBanks :: String -> Banks
getBanks string = Seq.fromList $ map read $ words string

findBank :: Banks -> Position
findBank banks = fromMaybe (-1) $ Seq.elemIndexL max banks
  where
    max = maximum banks

resetBank :: Position -> Banks -> Banks
resetBank position banks = Seq.update position 0 banks

updateBanks :: Position -> Count -> Banks -> Banks
updateBanks _ 0 banks = banks
updateBanks position count banks = updateBanks position nextCount nextBanks
  where
    size = Seq.length banks
    nextCount = count - 1
    nextBanks = Seq.adjust succ ((position + count) `mod` size) banks

distributeBank :: Position -> Banks -> Banks
distributeBank position banks =
  updateBanks position block $ resetBank position banks
  where
    block = Seq.index banks position

redistributeBanks :: Banks -> Banks
redistributeBanks banks = distributeBank position banks
  where
    position = findBank banks

countRedistributionCycles' :: Cycle -> States -> Banks -> (Banks, Cycle)
countRedistributionCycles' cycle states banks
  | banks `elem` states = (banks, cycle)
  | otherwise = countRedistributionCycles' nextCycle nextStates nextBanks
  where
    nextStates = Set.insert banks states
    nextBanks = redistributeBanks banks
    nextCycle = cycle + 1

countRedistributionCycles :: Banks -> (Banks, Cycle)
countRedistributionCycles banks = countRedistributionCycles' 0 Set.empty banks
