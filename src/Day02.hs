-- Inventory Management System
-- https://adventofcode.com/2018/day/2

module Day02
  ( preprocess,
    part1,
    part2
  ) where

import Data.List (group, sort, nub)

preprocess :: [String] -> [String]
preprocess = id

------------
-- Part 1 --
------------

part1 :: [String] -> (Int, Int)
part1 ids = (count 2, count 3)
  where count n = length $ filter (n `elem`) counts
        counts  = fmap lettercounts ids

        -- "icjjjbroqtualeyzpdmfksahgw" -> [2,1,3]
        lettercounts :: String -> [Int]
        lettercounts = nub . fmap length . group . sort


------------
-- Part 2 --
------------

part2 :: [String] -> ((String, String), Int)
part2 ss = head $ filter ((==1) . snd)
                $ [ ((a, b), length $ diffs a b) | a <- ss,
                                                   b <- ss ]

-- Reduce the first string to the chars that differ position-wise from the second string
--
-- diffs "foobar" "footar" -> "b"
diffs :: String -> String -> String
diffs as bs = map fst $ filter (uncurry (/=)) $ zip as bs

