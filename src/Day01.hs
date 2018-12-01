-- Chronal Calibration
-- https://adventofcode.com/2018/day/1

module Day01
  ( preprocess,
    part1,
    part2
  ) where

import Data.IntSet (member, insert, empty)

preprocess :: [String] -> [Int]
preprocess = map (read . trimPlus)
  where trimPlus = dropWhile (=='+')

------------
-- Part 1 --
------------

part1 :: [Int] -> Int
part1 = sum

------------
-- Part 2 --
------------

type Frequency  = Int
type FreqChange = Int

-- Find the first frequency we arrive at for the second time
part2 :: [FreqChange] -> Frequency
part2 cs = go (cycle cs) empty 0
  where go (c:cs) set freq
          | member (freq+c) set = freq+c
          | otherwise           = go cs (insert (freq+c) set) (freq+c)

