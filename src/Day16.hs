-- Chronal Classification
-- https://adventofcode.com/2018/day/16

module Day16
  ( preprocess,
    part1,
    part2,
    Input
  ) where

import Control.Applicative (many)
import NanoParsec

type Registers   = [Int]
type Instruction = (Int, Int, Int, Int)
type Sample      = (Registers, Instruction, Registers)
type Input       = ([Sample], [Instruction])

preprocess :: [String] -> Input
preprocess = run input . unlines

-- Before: [1, 2, 3, 4]
-- After:  [1, 2, 3, 4]
before, after :: Parser Registers
before = string "Before: " *> registers
after  = string "After:  " *> registers

-- [1, 2, 3, 4]
registers :: Parser Registers
registers = char '[' *> (number <-> string ", ") <* char ']'

-- 7 2 0 0
instruction :: Parser (Int, Int, Int, Int)
instruction = do
  opcode <- number; space
  a      <- number; space
  b      <- number; space
  output <- number
  return $ (opcode, a, b, output)

-- Before: [0, 1, 2, 1]
-- 12 3 2 2
-- After:  [0, 1, 1, 1]
sample :: Parser Sample
sample = (,,) <$> (before      <* nl)
              <*> (instruction <* nl)
              <*> (after       <* nl)
  
-- Parse the whole input as one line
input :: Parser Input
input = (,) <$> (sample      <-> nl) <* (nl >> nl >> nl)
            <*> (instruction <-> nl) <* nl

nl :: Parser Char
nl = char '\n'

(<->) = sepBy


------------
-- Part 1 --
------------

part1 :: Input -> Input
part1 = id


------------
-- Part 2 --
------------

part2 :: Input -> String
part2 input = "foo"

