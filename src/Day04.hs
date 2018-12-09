-- Repose Record
-- https://adventofcode.com/2018/day/4

{-# LANGUAGE TupleSections #-}

module Day04
  ( preprocess,
    part1,
    part2
  ) where

import qualified Data.IntMap as IntMap
import qualified Data.Map    as Map
import Control.Applicative ((<|>))
import Data.List           (maximumBy, sortBy)
import Data.Ord            (comparing)
import NanoParsec

-- In my puzzle input the year is constant, so we won't model it
data Timestamp = Timestamp { month :: Int,
                             day :: Int,
                             hour :: Int,
                             minute :: Int
                           } deriving (Eq, Ord, Show)

type BadgeID = Int
type Minute  = Int

data Record = BeginShift  Timestamp BadgeID
            | FallsAsleep Timestamp
            | WakesUp     Timestamp
            deriving (Show)

parseRecord :: Parser Record
parseRecord = begins <|> fallsAsleep <|> wakesUp

-- [1518-11-01 00:00] Guard #10 begins shift
-- [1518-11-01 00:05] falls asleep
-- [1518-11-01 00:25] wakes up
begins, fallsAsleep, wakesUp :: Parser Record
begins      = BeginShift  <$> timestamp <* string " Guard #" <*> number <* string " begins shift"
fallsAsleep = FallsAsleep <$> timestamp <* string " falls asleep"
wakesUp     = WakesUp     <$> timestamp <* string " wakes up"

timestamp :: Parser Timestamp
timestamp = do
  string "["
  _      <- number; string "-"
  month  <- number; string "-"
  day    <- number; space
  hour   <- number; string ":"
  minute <- number; string "]"
  return $ Timestamp month day hour minute
  
getTimestamp :: Record -> Timestamp
getTimestamp (BeginShift  ts _) = ts
getTimestamp (FallsAsleep ts)   = ts
getTimestamp (WakesUp     ts)   = ts

-- Convert our ASCII lines into Record objects, one per line, and sort by timestamp
preprocess :: [String] -> [Record]
preprocess = sortBy (comparing getTimestamp) . map (run parseRecord)


------------
-- Part 1 --
------------

-- Find the guard that has the most minutes asleep, and the minute they're asleep the most
part1 :: [Record] -> (BadgeID, Minute)
part1 rs = let slept = analyze rs
               guard = fst
                         $ maximumBy (comparing snd) 
                         $ IntMap.toList 
                         $ IntMap.fromListWith (+)
                         $ map (fmap (const 1)) -- convert the minute number to 1 minute the duration
                         $ slept
               min   = fst
                         $ maximumBy (comparing snd)
                         $ IntMap.toList
                         $ IntMap.fromListWith (+)
                         $ map ((,1) . snd)
                         $ filter ((==guard) . fst)
                         $ slept
           in  (guard, min)

-- Analyze a (sorted) list of records to determine which minutes
-- of the night were slept through by which guards
analyze :: [Record] -> [(BadgeID, Minute)]
analyze rs = go 0 rs

  where go :: BadgeID -> [Record] -> [(BadgeID, Minute)]

        -- Switch the guard we're analyzing
        go _ (BeginShift _ badge : rs) = go badge rs

        -- A FallsAsleep record is always followed by a WakesUp record, so pattern match on this pair
        go badge (FallsAsleep a : WakesUp w : rs) 
            = minutes badge a w ++ go badge rs

        go _ [] = []

        -- In our puzzle input, guards don't fall asleep before midnight, which simplifies this
        minutes b a w = [ (b, minute a + i) | i <- [0 .. minute w - minute a - 1] ]


------------
-- Part 2 --
------------

-- Which guard is most frequently asleep on the same minute, and which minute?
part2 :: [Record] -> (BadgeID, Minute)
part2 rs = fst $ head
               $ sortBy (flip (comparing snd))
               $ Map.toList
               $ Map.fromListWith (+)
               $ map (,1)
               $ analyze rs

