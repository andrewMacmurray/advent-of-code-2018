module Day2 where

import           Data.List (lines)
import qualified Data.Map  as Map
import qualified Data.Set  as Set
import           System.IO (readFile)

solution1 :: IO Int
solution1 = checksum <$> fromFile

checksum :: [String] -> Int
checksum = product . Map.elems . totalCount . map letterCount

totalCount :: [Set.Set Int] -> Map.Map Int Int
totalCount = collectLetterCounts mempty
  where
    collectLetterCounts = foldl $ foldr accum
    accum count = Map.insertWith (+) count 1

letterCount :: String -> Set.Set Int
letterCount = countsAbove1 . foldr accum mempty
  where
    countsAbove1 = Set.filter (> 1) . Set.fromList . Map.elems
    accum letter = Map.insertWith (+) letter 1

fromFile :: IO [String]
fromFile = lines <$> readFile "resources/day2-box-ids.txt"
