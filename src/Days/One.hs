module Days.One
  ( solution1
  , solution2
  ) where

import qualified Data.Set as S
import           Util     (fromFile)

solution1 :: IO Int
solution1 = sum <$> frequencies

solution2 :: IO Int
solution2 = firstDuplicate S.empty 0 . cycle <$> frequencies

firstDuplicate :: S.Set Int -> Int -> [Int] -> Int
firstDuplicate seen current (x:xs) =
  if duplicate
    then frequency
    else firstDuplicate (frequency `S.insert` seen) frequency xs
  where
    frequency = current + x
    duplicate = frequency `S.member` seen

frequencies :: IO [Int]
frequencies = fmap frequency <$> fromFile "resources/day-1-frequency.txt"
  where
    frequency ('+':n) = read n
    frequency n       = read n
