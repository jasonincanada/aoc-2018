-- Chronal Coordinates
-- https://adventofcode.com/2018/day/6

{-# LANGUAGE TupleSections #-}

module Day06
  ( preprocess,
    part1,
    part2
  ) where

import Data.Char  (isDigit)
import Data.List  (delete, sortBy, (\\))
import Data.Maybe (mapMaybe)
import Data.Ord   (comparing)
import qualified Data.Map as Map

type Coord  = (Int, Int)
type Point  = (Int, Int)
type Points = [Point]
type Area   = Int

preprocess :: [String] -> [Point]
preprocess = map tuplify
  where tuplify s = let x = read $ takeWhile isDigit s
                        y = read $ tail $ dropWhile (/=',') s
                    in  (x, y)

------------
-- Part 1 --
------------

part1 :: [Point] -> (Point, Area)
part1 ps = fmap (+1)
             $ head
             $ sortBy (flip (comparing snd))
             $ Map.toList
             $ Map.fromListWith (+)
             $ map (,1)
             $ filter (not . flip elem (infinities ps))
             $ mapMaybe (closest ps) (uncharted ps)

-- List the coordinates within our bounding box
uncharted :: [Point] -> [Coord]
uncharted ps = let xs     = map fst ps
                   ys     = map snd ps
                   x      = minimum xs
                   y      = minimum ys
                   width  = maximum xs - x
                   height = maximum ys - y
                   range  = [ (x+dx, y+dy) | dx <- [0.. width],
                                             dy <- [0..height] ]
               in  range \\ ps

-- Any point that is the closest one to a sufficiently far off coordinate
-- is considered to have infinite area
infinities :: [Point] -> [Point]
infinities ps = filter hasInfiniteArea ps
  where hasInfiniteArea point = point `elem` (mapMaybe (closest ps) (probes point))
        probes (x, y)         = [ (x+dx, y+dy) | (dx, dy) <- [(0, probeLength),
                                                              (0,-probeLength),
                                                              ( probeLength, 0),
                                                              (-probeLength, 0)] ]
        probeLength           = 10000

-- Find the point closest to a given coordinate, or nothing if there's a tie
closest :: [Point] -> Coord -> Maybe Point
closest ps point = let withoutPoint = delete point ps
                       dtp a        = distanceTo point a
                       (a:b:_)      = sortBy (comparing dtp) withoutPoint
                   in  if dtp a == dtp b
                       then Nothing
                       else Just a

-- Manhattan distance
distanceTo :: Coord -> Coord -> Int
distanceTo (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

------------
-- Part 2 --
------------

part2 :: [Point] -> [Point]
part2 ps = []

