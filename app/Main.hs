{- Advent of Code 2018

   URL: http://adventofcode.com/2018
-}

module Main where

import AdventOfCode (getDayInput)
import Day16 (preprocess, part1, part2)
import qualified Data.Map as Map

day = 16

main :: IO ()
main = do
  fileLines <- getDayInput day
  let pre = preprocess fileLines
  putStrLn $ show (part1 pre)
  putStrLn $ show (part2 pre)

