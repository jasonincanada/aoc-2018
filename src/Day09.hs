-- Marble Mania
-- https://adventofcode.com/2018/day/9

module Day09
  ( preprocess,
    part1,
    part2
  ) where

import Data.Ord  (comparing)
import Data.List (sortBy)
import qualified Data.IntMap as IntMap
import qualified Data.List.PointedList as PL
import Data.List.PointedList.Circular (next, previous)

preprocess :: [String] -> (Int, Int)
preprocess s = (people, marbles)
  where people  = read (ws !! 0)
        marbles = read (ws !! 6)
        ws      = words (head s)

------------
-- Part 1 --
------------

type Player = Int
type Round  = Int

part1 :: (Int, Int) -> [(Player, Int)]
part1 (people, marbles) = take 5
                            $ reverse
                            $ sortBy (comparing snd)
                            $ IntMap.toList
                            $ IntMap.fromListWith (+)
                            $ go 1 1 start

  where start = let Just list = PL.fromList [0]
                in  list

        go :: Player -> Round -> PL.PointedList Int -> [(Player, Int)]
        go p r list
          | r > marbles     = []
          | r `mod` 23 == 0 = (p, r)
                                : (p, sevenAgo list)
                                : go ((p+1) `mod` people) (r+1) (prune list)
          | otherwise       =     go ((p+1) `mod` people) (r+1) (insert r list)

        -- Skip a marble then insert a new one after it
        insert :: Int -> PL.PointedList Int -> PL.PointedList Int
        insert n = PL.insert n . next

        -- Get the value of the marble 7 to the left
        sevenAgo :: PL.PointedList Int -> Int
        sevenAgo = get . head . drop 7 . iterate previous
          where get (PL.PointedList _ f _) = f

        -- Drop the marble 7 to the left, retain the new focus there
        prune :: PL.PointedList Int -> PL.PointedList Int
        prune = delete . head . drop 7 . iterate previous
          where delete l = let Just list = PL.delete l
                           in  list


------------
-- Part 2 --
------------

part2 :: (Int, Int) -> [(Player, Int)]
part2 = part1 . fmap (*100)

{-
  jason@ubuntu16:~/aoc-2018$ time stack exec aoc2018-exe
  [(167,361466),(297,361017),(90,359588),(374,357662),(298,353807)]
  [(45,2945918550),(482,2943153973),(221,2943015432),(336,2942966844),(423,2941621319)]

  real    0m13.298s
  user    0m11.880s
  sys     0m0.720s
-}
