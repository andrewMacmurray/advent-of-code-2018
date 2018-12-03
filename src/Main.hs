module Main where

import qualified Days.One
import qualified Days.Three
import qualified Days.Two

main :: IO ()
main = Days.Three.solution1 >>= print
