-- No Matter How You Slice It
-- https://adventofcode.com/2018/day/3

{-# LANGUAGE TupleSections #-}

module Day03
  ( preprocess,
    part1,
    part2
  ) where

import qualified Data.IntMap.Strict as IntMap
import           NanoParsec

data Claim = Claim { num    :: Int,
                     left   :: Int,
                     top    :: Int,
                     width  :: Int,
                     height :: Int
                   } deriving (Show)

-- #1 @ 596,731: 11x27
parseClaim :: Parser Claim
parseClaim = do
  string "#"
  num    <- number; string " @ "
  left   <- number; string ","
  top    <- number; string ": "
  width  <- number; string "x"
  height <- number
  return $ Claim num left top width height
  
-- Convert our ASCII lines into Claim objects, one per line
preprocess :: [String] -> [Claim]
preprocess = map (run parseClaim)

------------
-- Part 1 --
------------

-- Count the number of tiles occupied by two or more claims
part1 :: [Claim] -> Int
part1 claims = length
                 $ filter (>=2)
                 $ IntMap.elems 
                 $ toMap claims

-- Convert a list of claims into an IntMap with coordinates for keys and
-- number of claims on a given coordinate as values
toMap :: [Claim] -> IntMap.IntMap Int
toMap claims = IntMap.fromListWith (+)
                 $ map (,1)
                 $ concatMap keys claims

-- Construct the list of keys representing the coordinates covered by a claim.
-- Arbitrarily choose 2000 as the width of a row (it's really only a bit more
-- than 1000 but 2000 looks cleaner than e.g. 1015 and works just as well. A
-- preprocessing pass to compute the actual width is overkill for this soln)
keys :: Claim -> [Int]
keys c = let t = top  c
             l = left c
         in  [ 2000*(t+row) + l+col | row <- [0 .. height c - 1],
                                      col <- [0 .. width  c - 1] ]

------------
-- Part 2 --
------------

part2 :: [Claim] -> [Claim]
part2 claims = filter (not . isOverlapped grid) claims
  where grid = toMap claims

isOverlapped :: IntMap.IntMap Int -> Claim -> Bool
isOverlapped grid c = any (>1) 
                        $ map (grid IntMap.!)
                        $ keys c

