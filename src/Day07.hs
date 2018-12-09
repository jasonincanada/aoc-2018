-- The Sum of Its Parts
-- https://adventofcode.com/2018/day/7

module Day07
  ( preprocess,
    part1,
    part2
  ) where

import Data.List (delete, nub, sort, (\\))
import qualified Data.Map as Map

type Step  = Char
type Link  = (Step, Step)
type Trail = [Step]
type Graph = Map.Map Step [Step] 

preprocess :: [String] -> [Link]
preprocess = map linkify
  where linkify s = (s !! 5, s !! 36)

------------
-- Part 1 --
------------

part1 :: [Link] -> Trail
part1 ls = go (toMap ls, "")
  where go (g, t) 
          | Map.size g == 0 = reverse t
          | otherwise       = go $ doStep (g, t)

steps :: [Link] -> [Step]
steps ls = nub $ concatMap toSteps ls
  where toSteps ls = [fst ls, snd ls]

doStep :: (Graph, Trail) -> (Graph, Trail)
doStep (g, t) = let as = availables g
                in if null as
                   then (g, t)
                   else let h = minimum as
                        in  (remove h g, h : t) 

remove :: Step -> Graph -> Graph
remove s g = removeEmpties $ Map.map (delete s) g
  where removeEmpties = Map.fromList
                          . filter (not . null . snd)
                          . Map.toList

-- Determine which steps in the graph are available to be processed next
availables :: Graph -> [Step]
availables g = nub (concat $ Map.elems g) \\ Map.keys g

-- Build a map keyed on step with values all the steps that link to it
toMap :: [Link] -> Map.Map Step [Step]
toMap = Map.fromListWith (++)
          . map tuplify

tuplify :: (a, b) -> (b, [a])
tuplify (x, y) = (y, [x])


------------
-- Part 2 --
------------

part2 :: [Link] -> [Link]
part2 = id

