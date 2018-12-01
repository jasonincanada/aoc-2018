{- Advent of Code 2018

   Day: 1 - Chronal Calibration - Part 2
   URL: http://adventofcode.com/2018/day/1#part2
-}

import Data.IntSet (member, insert, empty)

type Frequency  = Int
type FreqChange = Int

-- Haskell's read doesn't like the + sign so remove it while converting to FreqChanges
pre :: [String] -> [FreqChange]
pre = map (read . trimPlus)
  where trimPlus ('+':xs) = xs
        trimPlus xs       = xs

-- Find the first frequency we arrive at for the second time
process :: [FreqChange] -> Frequency
process cs = go (cycle cs) empty 0
  where go (c:cs) set freq
          | member (freq+c) set = freq+c
          | otherwise           = go cs (insert (freq+c) set) (freq+c)

main :: IO ()
main = do
  file <- readFile "inputs/1.txt"
  let numbers = lines file
  let prep    = pre numbers
  let day1b   = process prep
  putStrLn $ show day1b
