module Day1
  ( solution1
  , solution2
  ) where

import           Data.List (lines)
import qualified Data.Set  as Set
import           System.IO (readFile)

solution1 :: IO Int
solution1 = sum <$> frequencies

solution2 :: IO Int
solution2 = findDuplicate Set.empty 0 . cycle <$> frequencies

findDuplicate :: Set.Set Int -> Int -> [Int] -> Int
findDuplicate seen current (x:xs) =
  if appearsTwice
    then frequency
    else findDuplicate frequencies frequency xs
  where
    frequency = current + x
    appearsTwice = Set.member frequency seen
    frequencies = Set.insert frequency seen

frequencies :: IO [Int]
frequencies = fmap frequency . lines <$> fromFile
  where
    frequency ('+':n) = read n
    frequency n       = read n

fromFile :: IO String
fromFile = readFile "resources/day-1-frequency.txt"
