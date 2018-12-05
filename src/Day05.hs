-- Alchemical Reduction
-- https://adventofcode.com/2018/day/5

module Day05
  ( preprocess,
    part1,
    part2
  ) where

import Data.Char (ord, toLower)

preprocess :: [String] -> String
preprocess = head

------------
-- Part 1 --
------------

part1 :: String -> Int
part1 = length . reduce

reduce :: String -> String
reduce = firstRepeat . iterate zap2

-- Iterate until we find the fixed point of zap2, in other words, the string
-- is staying the same length because no further reductions are taking place
firstRepeat :: [[a]] -> [a]
firstRepeat (as:bs:rest)
  | length as == length bs  = as
  | otherwise               = firstRepeat (bs:rest)

-- We have to zap twice per round, once to test for the "even" pairings
-- and once for the "odd" pairings
zap2 :: String -> String
zap2 [] = []
zap2 as = zap $ head as : zap (tail as)

zap :: String -> String
zap []  = []
zap [a] = [a]
zap (a:b:as)
  | zappable a b =     zap as
  | otherwise    = a : zap (b:as)

zappable :: Char -> Char -> Bool
zappable a b = intify a + intify b == 0
  where intify x
          | x `elem` ['a'..'z'] = -1 * (ord x - ord 'a' + 1)
          | x `elem` ['A'..'Z'] =  1 * (ord x - ord 'A' + 1)

------------
-- Part 2 --
------------

part2 :: String -> [(Char, Int)]
part2 s = [ (c, part1 $ remove c s) | c <- ['a'..'z'] ]
  where remove c = filter ((/=c) . toLower)

