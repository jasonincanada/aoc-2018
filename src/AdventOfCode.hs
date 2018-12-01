{- Advent of Code 2018

   URL: http://adventofcode.com/2018
-}

module AdventOfCode 
  ( getDayInput
  ) where

getDayInput :: Int -> IO [String]
getDayInput day = lines <$> readFile (fileName day)

fileName :: Int -> String
fileName day = "inputs/" ++ show day ++ ".txt"

