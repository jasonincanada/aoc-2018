{- Advent of Code 2018

   URL: http://adventofcode.com/2018
-}

module Main where

import AdventOfCode (getDayInput)
import Day10 (preprocess, part1, part2)

day = 10

main :: IO ()
main = do
  fileLines <- getDayInput day
  let pre = preprocess fileLines
  mapM_ putStrLn $ part1 pre
  putStrLn $ show (part2 pre)

