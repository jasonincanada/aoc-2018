{- Advent of Code 2018

   URL: http://adventofcode.com/2018
-}

module AdventOfCode 
  ( getDayInput
  ) where

getDayInput :: Int -> IO [String]
getDayInput day = do
  contents <- readFile ("inputs/" ++ show day ++ ".txt")
  return $ lines contents

