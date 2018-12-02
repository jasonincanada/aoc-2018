{- Advent of Code 2018

   URL: http://adventofcode.com/2018
-}

module Main where

import AdventOfCode (getDayInput)
import Day02 (preprocess, part1, part2)

day = 2

main :: IO ()
main = do
  fileLines <- getDayInput day
  let pre = preprocess fileLines
  putStrLn $ show (part1 pre)
  putStrLn $ show (part2 pre)

