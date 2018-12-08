-- Memory Maneuver
-- https://adventofcode.com/2018/day/8

module Day08
  ( preprocess,
    part1,
    part2
  ) where

import Control.Applicative (many)
import Control.Monad       (replicateM)
import NanoParsec

type Metadata = Int

data Tree = Tree [Tree] [Metadata]

preprocess :: [String] -> String
preprocess = head

------------
-- Part 1 --
------------

part1 :: String -> Int
part1 = sum . metadatas . run node

node :: Parser Tree
node = do
  childCount    <- number <* space
  metadataCount <- number <* space

  childs    <- replicateM childCount    node
  metadatas <- replicateM metadataCount (number <* many space)

  return $ Tree childs metadatas

-- List only the metadata entries throughout the tree
metadatas :: Tree -> [Metadata]
metadatas (Tree cs ms) = concatMap metadatas cs ++ ms

------------
-- Part 2 --
------------

part2 :: String -> Int
part2 = value . run node

value :: Tree -> Int
value (Tree childs metadatas)
  | null childs = sum metadatas
  | otherwise   = sum [ value (childs !! (m-1)) | m <- metadatas,
                                                  1 <= m && m <= length childs ]
