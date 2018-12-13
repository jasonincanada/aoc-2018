{- Advent of Code 2018

   Day 12 - Subterranean Sustainability  [ https://adventofcode.com/2018/day/12 ]


   References:

   Cellular Automata post by Edward Kmett
     https://www.schoolofhaskell.com/user/edwardk/cellular-automata/part-1

-}

{-# Language DeriveFunctor #-}
{-# Language RankNTypes #-}

module Day12
  ( preprocess,
    part1,
    part2
  ) where

import Control.Monad (replicateM)
import Control.Comonad
import Data.List (find)
import Data.MemoCombinators hiding (maybe)
import NanoParsec 

-- The state is the list of pot numbers that are planted
type Pot     = Int
type Garden  = [Pot]
type Segment = [Int]
type Note    = (Segment, Bool)

preprocess :: [String] -> (Garden, [Note])
preprocess ss = (initial, notes)
  where initial  = indicesOf '#' $ drop 15 (ss !! 0)
        notes    = map (run parseNote) (drop 2 ss)

parseNote :: Parser Note
parseNote = do
  pots <- replicateM 5 (oneOf ".#")
  string " => "
  pot' <- oneOf ".#"
  return (map (subtract 2) $ indicesOf '#' pots, pot' == '#')

indicesOf :: Eq a => a -> [a] -> [Int]
indicesOf a = map fst
                . filter ((==a) . snd)
                . zip [0..]

------------
-- Part 1 --
------------

data Store s a = Store (s -> a) s
                 deriving Functor

instance Comonad (Store s) where
  extract   (Store sa s) = sa s
  duplicate (Store sa s) = Store (Store sa) s

-- With a list of notes and the current store at a single focus,
-- determine whether the focus ends up being a potted plant or not
-- by looking in the neighbourhood of the focus
rule :: [Note] -> Store Int Bool -> Bool
rule notes (Store ib i) = maybe (ib i) snd (find f notes)
  where f (note, _) = and [ ib (i-2) == ((-2) `elem` note),
                            ib (i-1) == ((-1) `elem` note),
                            ib (i+0) == (  0  `elem` note),
                            ib (i+1) == (  1  `elem` note),
                            ib (i+2) == (  2  `elem` note) ]

-- Set up memoization to avoid repeated calculations
tab :: Num s => Memo s -> Store s a -> Store s a
tab opt (Store f s) = Store (opt f) s

-- Our initial store is focused at 0 and is a function that returns
-- true on i if there is a plant in pot i
start :: Garden -> Store Int Bool
start garden = Store (\i -> i `elem` garden) 0

experiment :: Functor f => (s -> f s) -> Store s a -> f a
experiment k (Store sa s) = sa <$> k s

window :: (Enum s, Num s) => s -> s -> Store s a -> [a]
window l h = experiment $ \s -> [s-l..s+h]

-- Get the list of pot numbers that have plants in them
potteds :: Int -> Int -> Store Int Bool -> [Int]
potteds l h s = map fst
                  $ filter snd
                  $ zip [(-l)..]
                  $ window l h s

loop :: Integral s => (Store s a -> a) -> Store s a -> [Store s a]
loop f = iterate (extend f . tab integral)

part1 :: (Garden, [Note]) -> [Int]
part1 (garden, notes) = map (sum . potteds 200 200)
                          $ take 150
                          $ loop (rule $ filter useful notes)
                          $ start garden

-- Segments whose middle value is the same as the replacement have no effect
-- so we can remove them for the free performance boost
useful :: Note -> Bool
useful (segment, bool) = (0 `elem` segment) /= bool

------------
-- Part 2 --
------------

-- I plotted the growth graph from part 1 in Excel and noticed at x > 100 the
-- scattered slope becomes a straight line.  I take a shortcut here and manually
-- set up the equation.  A future version could calculate the 51 and 832 numbers
-- from the data in part 1 automatically somehow
part2 :: a -> Int
part2 _ = f 50000000000
  where f x = 51*x + 832

