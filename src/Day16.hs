-- Chronal Classification
-- https://adventofcode.com/2018/day/16

module Day16
  ( preprocess,
    part1,
    part2,
    Input
  ) where

import Control.Applicative (many)
import Data.Bits           ((.&.), (.|.))
import Data.Bool           (bool)
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
  c      <- number
  return (opcode, a, b, c)

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

-- Set the value of register i.  This isn't an efficient way to update a list element in Haskell
-- but it saves on importing Data.Sequence and doing sequence/list conversions everywhere
set :: Registers -> Int -> Int -> Registers
set regs i val = go regs 0
  where go []     _   = []
        go (r:rs) n
          | n == i    = val : rs
          | otherwise = r   : go rs (n+1)

-- Carry out function f on two values, one or both of which may be read from a register,
-- and store the result in the register C from the instruction
regOp, valOp, lavOp :: (Int -> Int -> Int) -> Registers -> Instruction -> Registers
regOp f regs (_, a, b, c) = set regs c (f (regs !! a) (regs !! b))
valOp f regs (_, a, b, c) = set regs c (f (regs !! a)  b         )
lavOp f regs (_, a, b, c) = set regs c (f  a          (regs !! b))


type Operation = (Registers -> Instruction -> Registers)

-- Addition
addr, addi :: Operation
addr = regOp (+)
addi = valOp (+)

-- Multiplication
mulr = regOp (*)
muli = valOp (*)

-- Bitwise AND
banr = regOp (.&.)
bani = valOp (.&.)

-- Bitwise OR
borr = regOp (.|.)
bori = valOp (.|.)

-- Assignment
setr = regOp const
seti = lavOp const

-- Greater-than testing
gtrr = regOp (tester (>))
gtri = valOp (tester (>))
gtir = lavOp (tester (>))

-- Equality testing
eqrr = regOp (tester (==))
eqri = valOp (tester (==))
eqir = lavOp (tester (==))

tester :: (a -> a -> Bool) -> a -> a -> Int
tester f a b = bool 0 1 $ f a b

-- Collect all our operations
operations :: [Operation]
operations =  [addr, addi, mulr, muli,
               banr, bani, borr, bori,
               setr, seti,
               gtrr, gtri, gtir,
               eqrr, eqri, eqir]


{- Ignoring the opcode numbers, how many samples in your puzzle input behave like three
   or more operations? -}

part1 :: Input -> Int
part1 (samples, _) = length (filter (>=3) matches)
  where matches = [ sum $ map (testWith sample) operations | sample <- samples ]

        testWith :: Sample -> Operation -> Int
        testWith (before, ins, after) op = bool 0 1 (before `op` ins == after)


------------
-- Part 2 --
------------

part2 :: Input -> String
part2 input = "foo"

