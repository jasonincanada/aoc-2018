{- Advent of Code 2018

   Day 15 - Beverage Bandits  [ https://adventofcode.com/2018/day/15 ]

   How about this large plate of spagetti

   This code works but needs a lot of re-factoring. It could probably benefit from lenses,
   and a proper separation of the state machine into at least two separate components: 
   events/logic that drives it, and a state reducer.

   This was painful to write but it's a good program for meditating on improvements
-}

{-# Language TupleSections #-}

module Day15
  ( preprocess,
    part1,
    part2,
    log
  ) where

import           Data.Bool (bool)
import           Data.List (delete, find, sort, sortBy)
import           Data.Ord  (comparing)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Prelude hiding (log)

type Row       = Int
type Column    = Int
type Coord     = (Row, Column)
type Hitpoints = Int
type Walls     = Set.Set Coord

-- The unit ID is the coordinates we originally saw them at. This will stay the
-- same even though the unit is moving around the board
type UnitID    = Coord

data UnitType  = Goblin | Elf deriving (Eq, Show)
data Unit      = Unit UnitType Coord Hitpoints deriving (Show)

-- Key the units on their original coordinates
type Units     = Map.Map UnitID Unit

data Game      = Game { walls          :: Walls
                      , units          :: Units
                      , roundUnitIDs   :: [UnitID]
                      , currentUnitID  :: UnitID
                      , roundNumber    :: Int
                      , log            :: [String]
                      , elfDeaths      :: Int
                      , elfAttackPower :: Int
                      } deriving (Show)

preprocess :: [String] -> Game
preprocess ss = p withCoords [] []
  where p [] walls units              = Game 
                                          (Set.fromList walls)
                                          (Map.fromList units)
                                          []
                                          (0,0)
                                          1
                                          []
                                          0
                                          3 -- Part 2 is correct when this is 14
        p ((coord, '#') : rest) ws us = p rest (coord : ws) us
        p ((coord, 'E') : rest) ws us = p rest ws ((coord, Unit Elf    coord hp) : us)
        p ((coord, 'G') : rest) ws us = p rest ws ((coord, Unit Goblin coord hp) : us)
        p (_            : rest) ws us = p rest ws us

        hp              = 200
        withCoords      = map toCoords (zip [0..] $ concat ss)
        toCoords (n, x) = ((n `div` width, n `mod` width), x)
        width           = length (head ss)

------------
-- Part 1 --
------------

-- Get the 4 coordinates surrounding a unit
around :: Unit -> Set.Set Coord
around (Unit _ c _) = Set.fromList $ cardinals c

cardinals :: Coord -> [Coord]
cardinals coord = map (add coord) [ (0,-1), (-1,0), (1,0), (0,1) ]
  where add (r1,c1) (r2,c2) = (r1+r2, c1+c2)

coord :: Unit -> Coord
coord (Unit _ c _) = c

hp :: Unit -> Hitpoints
hp (Unit _ _ hp) = hp

isType :: UnitType -> Unit -> Bool
isType ty (Unit ty' _ _) = ty == ty'

-- True if there are targets left of the given type on the board
targetsLeft :: UnitType -> Game -> Bool
targetsLeft ty game = any (isType ty) (units game)

-- Manually set these since they never change
height = 32
width  = 32

-- Get the set of empty coordinates on the game board
spaces :: Game -> Set.Set Coord
spaces g = all \\\ walls g \\\ coords (units g)
  where all    = Set.fromList
                   $ concat 
                   $ [[ (row, col) | col <- [0..width-1]] | row <- [0..height-1]]

        coords = Set.fromList 
                   . map coord
                   . Map.elems

        (\\\)  = Set.difference

-- Return all empty cells next to enemy units
inRange :: UnitID -> UnitType -> Game -> Set.Set Coord
inRange uid unitType g = surrounds |^| spaces g
  where surrounds = foldr 
                      Set.union
                      Set.empty
                      sets

        -- List of sets of the 4 coordinates surrounding enemy units
        sets      = Map.elems          -- :: [Set Coord]
                      $ Map.map around -- :: Map UnitID (Set Coord)
                      $ Map.delete uid -- :: Map UnitID Unit  -- TODO: This shouldn't need to be here
                      $ Map.filter (not . isType unitType)
                      $ units g        -- :: Map UnitID Unit

        (|^|)     = Set.intersection

-- Priority list of targets immediately surrounding a unit player
targetsAround :: UnitID -> Game -> [UnitID]
targetsAround uid g = map fst orderedHP
  where
        -- Filter the unit list to only the other unit types
        enemies    = Map.filter
                       (not . isType ty)
                       (units g)

        -- Filter enemies to the ones surrounding the player
        surrounds  = Map.filter
                       (flip elem aroundUnit . coord)
                       enemies
        
        -- Order by lowest hitpoints
        orderedHP  = sortBy
                       (comparing (hp . snd))
                       (Map.toList surrounds)

        aroundUnit = cardinals (coord unit)

        unit          = (units g) Map.! uid
        (Unit ty _ _) = unit


-- Do a breadth-first search to find all reachable spaces from the passed coordinate
explore :: Coord -> Set.Set Coord -> [(Int, Coord)]
explore (row,col) uncharted = go (Set.delete (row,col) uncharted)
                                [(0, (row,col))]
                                []

  where go uncharted queue charted
          | Set.null uncharted = charted
          | null queue         = charted
          | otherwise          = let (n,coord) = head $ sortBy (comparing fst) queue
                                     around    = Set.fromList $ cardinals coord
                                     intr      = Set.intersection uncharted around
                                     us'       = Set.difference   uncharted intr
                                     queue'    = delete (n,coord) queue ++ map (n+1,) (Set.toList intr)
                                     charted'  = charted                ++ map (n+1,) (Set.toList intr)
                                 in go us' queue' charted'

part1 :: Game -> Game
part1 game = go $ Start game

data GameState = Start      Game
               | StartRound Game
               | NextUnit   Game
               | Turn       Game
               | Move       Game
               | Scan       Game
               | Attack     Game
               | EndTurn    Game
               | EndRound   Game
               | Halted     Game
               deriving (Show)

addLog :: String -> Game -> Game
addLog s game = game { log = s : log game }

-- This is a deterministic state machine so we don't need events, just state transitions
go :: GameState -> Game
go (Start game)      = go (StartRound $ addLog "Starting game" game)

-- Take a "snapshot" of where the units are at the start of each round and run through their
-- turns in reading order throughout the rest of the round, regardless of subsequent moves
go (StartRound game) = let unitIDs = map fst 
                                       $ sortBy (comparing (coord . snd))
                                       $ Map.toList
                                       $ units game
                           game'   = game {
                                       roundUnitIDs = unitIDs
                                     }
                       in  go $ NextUnit (addLog ("Starting round " ++ show (roundNumber game')) game')

go (NextUnit game)   = if null (roundUnitIDs game)
                       then go $ EndRound game
                       else let nextUnitID    = head $ roundUnitIDs game
                                roundUnitIDs' = tail $ roundUnitIDs game
                                found         = Map.lookup nextUnitID (units game)
                            in  case found of
                                  Nothing   -> go $ NextUnit $ game { roundUnitIDs = roundUnitIDs' }
                                  Just unit -> go $ Turn     $ addLog ("---- " ++ show nextUnitID ++ " ----")
                                                                 game { roundUnitIDs = roundUnitIDs',
                                                                        currentUnitID = nextUnitID }

go (Turn game)       = let uid         = currentUnitID game
                           ts          = targetsAround uid game
                       in  if null ts
                           then go (Scan game)
                           else go (Attack $ addLog (show uid ++ " Straight to attack") game)

go (Scan game)       = let uid         = currentUnitID game
                           Just unit   = Map.lookup uid (units game)
                           Unit ty _ _ = unit
                           ty'         = bool Goblin Elf (ty == Goblin)
                       in  if targetsLeft ty' game
                           then go (Move game)
                           else go (Halted $ addLog "Halting because no targets of other type left " game)

go (Move game)       = let uid         = currentUnitID game
                           Just unit   = Map.lookup uid (units game)
                           Unit ty coord hp = unit

                           -- Get the empty spaces on the game board
                           spaces'     = spaces game

                           -- Fan out and collect reachable coordinates and the number of steps away they are
                           explored    = explore coord spaces' -- :: [(Int, Coord)]

                           -- Get the blank cells surrounding enemy units
                           range       = inRange uid ty game -- :: Set Coord

                           -- Intersect range/explored to get a list of movement targets
                           filtered    = filter ((`Set.member` range) . snd) explored

                           -- Use the sortability of tuples to sort the reachable coordinates by reading order
                           sorted      = sort filtered

                       in  -- If no targets, end this player's turn
                           if null filtered 
                           then go (EndTurn $ addLog ("Skipping unit at " ++ show coord ++ " because no targets reachable") game)

                           else let -- This is the empty cell beside the enemy we're targeting
                                    coord' = snd $ head filtered
                                    newCo  = -- If we're right beside it, move immediately there
                                             if coord' `elem` (cardinals coord)
                                             then coord'
                                             else let -- Otherwise, fan out again but from the target this time
                                                      expl' = sort $ explore coord' spaces'
                                                      intr  = filter (flip elem (cardinals coord) . snd) expl'
                                                  in  snd $ head intr
                                    unit'  = Unit ty newCo hp
                                    units' = Map.adjust (const unit') uid (units game)
                                    game'  = game { units = units' }
                                    log    = "Move " ++ show coord ++ " to " ++ show newCo ++ " to get closer to " ++ show coord'
                                in  go (Attack $ addLog log game')

go (Attack game)     = let uid       = currentUnitID game
                           Just unit = Map.lookup uid (units game)
                           ts        = targetsAround uid game
                       in  if null ts
                           then go $ EndTurn game
                           else let targetID    = head ts
                                    Just target = Map.lookup targetID (units game)
                                    Unit ty _ _ = target
                                    damage      = attackPower unit game
                                    target'     = hitTarget target damage
                                    units'      = if isDead target'
                                                  then Map.delete targetID (units game)
                                                  else Map.adjust (const target') targetID (units game)
                                    log         = if isDead target'
                                                  then "Target " ++ show targetID ++ " is dead"
                                                  else "Target " ++ show targetID ++ " takes " ++ show damage ++ " damage, is now " ++ show target'
                                    elfDeathAdd = if isDead target' && ty == Elf
                                                  then 1
                                                  else 0
                                    game'       = game { 
                                                    units = units',
                                                    elfDeaths = (elfDeaths game) + elfDeathAdd
                                                  }
                                in  go (EndTurn $ addLog log game')
                                

go (EndTurn game)    = go (NextUnit game)

go (EndRound game)   = go (StartRound
                             $ addLog "---------------- End of round"
                             $ game { roundNumber = roundNumber game + 1 })

go (Halted game)     = let rounds      = roundNumber game - 1
                           hitpoints   = foldr ((+) . hp) 0 (units game)
                           winner      = snd 
                                           $ head
                                           $ Map.toList
                                           $ units game
                           Unit ty _ _ = winner
                           outcome     = " Outcome: " ++ show rounds ++
                                         " Elf Deaths: " ++ show (elfDeaths game) ++
                                         " Winner: " ++ show ty ++
                                         " Score: " ++ show hitpoints
                           shortLog  = take 10 (log $ addLog outcome game)
                       in  game { log = shortLog }

hitTarget :: Unit -> Int -> Unit
hitTarget (Unit ty co hp) dp = Unit ty co (hp-dp)

isDead :: Unit -> Bool
isDead (Unit _ _ hp) = hp <= 0

attackPower :: Unit -> Game -> Int
attackPower (Unit Goblin _ _) _    = 3
attackPower (Unit Elf    _ _) game = elfAttackPower game

------------
-- Part 2 --
------------

part2 :: Game -> Int
part2 game = 0

