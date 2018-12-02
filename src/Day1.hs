module Day1
  ( solution1
  , solution2
  ) where

import qualified Data.Set as S
import           Util     (fromFile)

solution1 :: IO Int
solution1 = sum <$> frequencies

solution2 :: IO Int
solution2 = findDuplicate S.empty 0 . cycle <$> frequencies

findDuplicate :: S.Set Int -> Int -> [Int] -> Int
findDuplicate seen current (x:xs) =
  if appearsTwice
    then frequency
    else findDuplicate frequencies frequency xs
  where
    frequency = current + x
    appearsTwice = S.member frequency seen
    frequencies = S.insert frequency seen

frequencies :: IO [Int]
frequencies = fmap frequency <$> fromFile "resources/day-1-frequency.txt"
  where
    frequency ('+':n) = read n
    frequency n       = read n
