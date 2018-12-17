{- Advent of Code 2018

   URL: http://adventofcode.com/2018
-}

module Main where

import AdventOfCode (getDayInput)
import Day15 (preprocess, part1, part2, log)
import qualified Data.Map as Map

day = 15

main :: IO ()
main = do
  fileLines <- getDayInput day
  let pre   = preprocess fileLines
  let game  = part1 pre
  mapM_ putStrLn $ (reverse $ Day15.log game)
  putStrLn $ show (part2 pre)

