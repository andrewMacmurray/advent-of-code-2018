module Day1
  ( solution
  ) where

import           Data.List (lines)
import           System.IO (readFile)

solution :: IO Int
solution = frequency <$> readFile "resources/day-1-frequency.txt"

frequency :: String -> Int
frequency = sum . fmap parseFrequency . lines
  where
    parseFrequency ('+':n) = read n
    parseFrequency n       = read n
