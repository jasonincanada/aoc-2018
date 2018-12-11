{- Advent of Code 2018

   Day 10 - The Stars Align  [ https://adventofcode.com/2018/day/10 ]

   This version uses the Golden Section Search algorithm, inspired by the reddit
   thread linked below, to quickly zero in on the minimum of the bounding box size
   function, parameterized by time.  It's similar to gradient descent search
   but for strictly unimodal functions with a known starting input interval.


   References:

   Wikipedia entry for golden section search
     https://en.wikipedia.org/wiki/Golden-section_search

   Reddit comment from /u/aphirst
     https://old.reddit.com/r/adventofcode/comments/a4skra/2018_day_10_solutions/ebir4pj/

-}

module Day10
  ( preprocess,
    part1,
    part2
  ) where

import Control.Applicative (many)
import NanoParsec

type Time        = Int
type Position    = (Int, Int)
type Velocity    = (Int, Int)
type Star        = (Position, Velocity)
type BoundingBox = (Position, Position)

preprocess :: [String] -> [Star]
preprocess = map (run star)

-- position=<-31135,  10618> velocity=< 3, -1>
star :: Parser Star
star = do
  string "position=<"; many space
  px <- number; char ','; many space
  py <- number; string "> velocity=<"; many space
  vx <- number; char ','; many space
  vy <- number; char '>'
  return ((px, py), (vx, vy))


------------
-- Part 1 --
------------

part1 :: [Star] -> [String]
part1 ss = draw
             $ positions
             $ stateAt ss
             $ part2 ss

positions :: [Star] -> [Position]
positions = map fst

-- Calculate the state at any arbitrary point in the future
stateAt :: [Star] -> Time -> [Star]
stateAt stars t = map (at t) stars
  where at t ((x,y), (dx, dy)) = ((x + t*dx, y + t*dy), (dx, dy))

-- Create an ASCII drawing of our message by plotting an X at each position
draw :: [Position] -> [String]
draw ps = [[ letter (x,y) | x <- [0..maxX]] 
                          | y <- [0..maxY]]

  where letter p = if p `elem` toOrigin
                   then 'X'
                   else '.'
        toOrigin = map (`sub` bounds) ps
        bounds   = box ps
        maxX     = maximum $ map fst toOrigin
        maxY     = maximum $ map snd toOrigin

        sub :: Position -> BoundingBox -> Position
        sub (x, y) ((bx,by), _) = (x - bx, y - by)


{-  A message appears in the sky for a brief moment

    X....X..XXXXXX..X....X..XXXXX...X.......XXXXX...X....X..X....X
    XX...X..X.......X....X..X....X..X.......X....X..X....X..X...X.
    XX...X..X........X..X...X....X..X.......X....X...X..X...X..X..
    X.X..X..X........X..X...X....X..X.......X....X...X..X...X.X...
    X.X..X..XXXXX.....XX....XXXXX...X.......XXXXX.....XX....XX....
    X..X.X..X.........XX....X.......X.......X..X......XX....XX....
    X..X.X..X........X..X...X.......X.......X...X....X..X...X.X...
    X...XX..X........X..X...X.......X.......X...X....X..X...X..X..
    X...XX..X.......X....X..X.......X.......X....X..X....X..X...X.
    X....X..XXXXXX..X....X..X.......XXXXXX..X....X..X....X..X....X
-}


------------
-- Part 2 --
------------

-- Find the number of iterations it takes for the message to converge
part2 :: [Star] -> Int
part2 stars = goldenSection f 0 20000 
  where f = size . box . positions . stateAt stars

-- Get the bounding box for a list of positions
box :: [Position] -> BoundingBox
box ls = ( (minimum xs, minimum ys),
           (maximum xs, maximum ys) )
  where xs = map fst ls
        ys = map snd ls

size :: BoundingBox -> Int
size ((x1, y1), (x2, y2)) = (abs (x1-x2)+1) * (abs (y1-y2)+1)

-- Golden section search for functions with integer domain, ported from
-- the python code on Wikipedia
goldenSection :: (Integral a, Ord b) => (a -> b) -> a -> a -> a
goldenSection f x1 x2
  | x2 - x1 <= 3 = x2 - 1
  | otherwise    = let c = x2 - (ceiling $ fromIntegral (x2-x1) / phi)
                       d = x1 + (floor   $ fromIntegral (x2-x1) / phi)
                   in  if f c < f d
                       then goldenSection f x1 d
                       else goldenSection f c x2

phi :: Double
phi = (1 + sqrt (fromIntegral 5)) / 2

{-
  jason@ubuntu16:~/aoc-2018$ time stack exec aoc2018-exe
  ...

  real    0m0.260s
  user    0m0.192s
  sys     0m0.044s
-}

