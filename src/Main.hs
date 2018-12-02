module Main where

import qualified Day1

main :: IO ()
main = do
  printSolution 1 1 Day1.solution1
  printSolution 1 2 Day1.solution2

printSolution :: Show a => Int -> Int -> IO a -> IO ()
printSolution day number solution = putStr message >> solution >>= print
  where
    message = mconcat ["Day ", show day, ", Solution ", show number, ": "]
