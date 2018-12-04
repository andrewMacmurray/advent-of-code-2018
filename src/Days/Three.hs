{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Days.Three
  ( solution1
  ) where

import           Data.Attoparsec.Combinator (sepBy)
import qualified Data.Attoparsec.Text       as A
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           Data.Text.IO               (readFile)
import           Prelude                    hiding (readFile)

data Claim = Claim
  { id     :: Int
  , left   :: Int
  , top    :: Int
  , width  :: Int
  , height :: Int
  } deriving (Eq, Show)

type Coord = (Int, Int)

solution1 :: IO (Either String Int)
solution1 = (fmap overlappingArea . A.parseOnly claims) <$> fromFile

overlappingArea :: [Claim] -> Int
overlappingArea =
  M.size . M.filter (> 1) . foldr (M.unionWith (+) . counts . toCoords) mempty

counts :: [Coord] -> M.Map Coord Int
counts = foldr (\coord -> M.insertWith (+) coord 1) mempty

toCoords :: Claim -> [Coord]
toCoords Claim {..} =
  [ (x, y)
  | x <- [(left + 1) .. (left + width)]
  , y <- [(top + 1) .. (top + height)]
  ]

claims :: A.Parser [Claim]
claims = claim `sepBy` A.char '\n'

claim :: A.Parser Claim
claim =
  Claim <$> after "#" <*> after " @ " <*> after "," <*> after ": " <*> after "x"
  where
    after x = A.string x >> A.decimal

fromFile :: IO T.Text
fromFile = readFile "resources/day-3-claims.txt"
