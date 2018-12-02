module Day2 where

import qualified Data.Map as M
import qualified Data.Set as S
import           Util     (fromFile, nonEmpty)

-- Solution 2
type LetterPositions = M.Map Int Char

solution2 :: IO String
solution2 = findLetterDifference <$> boxIds

findLetterDifference :: [String] -> String
findLetterDifference ids = M.elems $ foldr nonEmpties mempty allPositions
  where
    allPositions = map letterPositions ids
    nonEmpties positions acc
      | nonEmpty acc = acc
      | otherwise = getSames positions allPositions

getSames :: LetterPositions -> [LetterPositions] -> LetterPositions
getSames boxId = foldr (collectSame boxId) mempty
  where
    collectSame idA idB acc
      | nonEmpty acc = acc
      | M.size (sameLetters idA idB) == M.size idA - 1 = sameLetters idA idB
      | otherwise = mempty

sameLetters :: LetterPositions -> LetterPositions -> LetterPositions
sameLetters = M.differenceWith checkForSame
  where
    checkForSame a b
      | a == b = Just a
      | otherwise = Nothing

letterPositions :: String -> LetterPositions
letterPositions = M.fromList . zip [0 ..]

-- Solution 1
solution1 :: IO Int
solution1 = checksum <$> boxIds

checksum :: [String] -> Int
checksum = product . M.elems . totalCount . map letterCount

totalCount :: [S.Set Int] -> M.Map Int Int
totalCount = collectLetterCounts mempty
  where
    collectLetterCounts = foldl $ foldr accum
    accum count = M.insertWith (+) count 1

letterCount :: String -> S.Set Int
letterCount = countsAbove1 . foldr accum mempty
  where
    countsAbove1 = S.filter (> 1) . S.fromList . M.elems
    accum letter = M.insertWith (+) letter 1

boxIds :: IO [String]
boxIds = fromFile "resources/day2-box-ids.txt"
