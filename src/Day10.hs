-- The Stars Align
-- https://adventofcode.com/2018/day/10

module Day10
  ( preprocess,
    part1,
    part2
  ) where

import Control.Applicative (many)
import NanoParsec

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
part1 = draw
          . snd
          . fmap positionsOnly
          . beforeIncrease (size . box . map fst . snd)
          . zip [0..]
          . iterate step 

  where positionsOnly :: [Star] -> [Position]
        positionsOnly = map fst

-- Step our physics simulation by one iteration
step :: [Star] -> [Star]
step = map advance
  where advance ((x, y), (dx, dy)) = ((x+dx, y+dy), (dx, dy))

-- Return the last item seen before a function on list elements increases for the first time
beforeIncrease :: Ord b => (a -> b) -> [a] -> a
beforeIncrease f list@(x:y:_)
  | f y > f x = head list
  | otherwise = beforeIncrease f $ tail list

-- Get the bounding box for a list of positions
box :: [Position] -> BoundingBox
box ls = ((x1, y1), (x2, y2))
  where x1 = minimum $ map fst ls
        y1 = minimum $ map snd ls
        x2 = maximum $ map fst ls
        y2 = maximum $ map snd ls

size :: BoundingBox -> Int
size ((x1, y1), (x2, y2)) = (abs (x1-x2)+1) * (abs (y1-y2)+1)

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

-- Count the number of iterations it takes for the message to converge
part2 :: [Star] -> Int
part2 = fst
          . beforeIncrease (size . box . map fst . snd)
          . zip [0..]
          . iterate step 

