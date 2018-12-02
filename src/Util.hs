module Util where

import           Data.List (lines)
import           System.IO (readFile)

fromFile :: String -> IO [String]
fromFile name = lines <$> readFile name

nonEmpty :: Foldable f => f a -> Bool
nonEmpty = not . null

indexed :: [a] -> [(Int, a)]
indexed = zip [0 ..]
