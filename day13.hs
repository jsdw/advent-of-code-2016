#!/usr/bin/env stack
-- stack --resolver lts-7.13 --install-ghc runghc --package attoparsec --package vector --package containers --package mtl

{-# LANGUAGE OverloadedStrings #-}

{-

--- Day 13: A Maze of Twisty Little Cubicles ---

You arrive at the first floor of this new building to discover a much less welcoming environment than the shiny atrium of the last one. Instead, you are in a maze of twisty little cubicles, all alike.

Every location in this area is addressed by a pair of non-negative integers (x,y). Each such coordinate is either a wall or an open space. You can't move diagonally. The cube maze starts at 0,0 and seems to extend infinitely toward positive x and y; negative values are invalid, as they represent a location outside the building. You are in a small waiting area at 1,1.

While it seems chaotic, a nearby morale-boosting poster explains, the layout is actually quite logical. You can determine whether a given x,y coordinate will be a wall or an open space using a simple system:

Find x*x + 3*x + 2*x*y + y + y*y.
Add the office designer's favorite number (your puzzle input).
Find the binary representation of that sum; count the number of bits that are 1.
If the number of bits that are 1 is even, it's an open space.
If the number of bits that are 1 is odd, it's a wall.
For example, if the office designer's favorite number were 10, drawing walls as # and open spaces as ., the corner of the building containing 0,0 would look like this:

  0123456789
0 .#.####.##
1 ..#..#...#
2 #....##...
3 ###.#.###.
4 .##..#..#.
5 ..##....#.
6 #...##.###
Now, suppose you wanted to reach 7,4. The shortest route you could take is marked as O:

  0123456789
0 .#.####.##
1 .O#..#...#
2 #OOO.##...
3 ###O#.###.
4 .##OO#OO#.
5 ..##OOO.#.
6 #...##.###
Thus, reaching 7,4 would take a minimum of 11 steps (starting from your current location, 1,1).

What is the fewest number of steps required for you to reach 31,39?

--- Part Two ---

How many locations (distinct x,y coordinates, including your starting location) can you reach in at most 50 steps?

-}

import qualified Data.Bits as Bits
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Ord (comparing)
import Debug.Trace

-- 1. The rules of the maze
--

puzzleInput = 1350

isOpenSpace :: (Int,Int) -> Bool
isOpenSpace (x,y) = (Bits.popCount equation `mod` 2) == 0
  where equation = x*x + 3*x + 2*x*y + y + y*y + puzzleInput

nextPositions :: (Int,Int) -> [(Int,Int)]
nextPositions (x,y) =
    filter isOpenSpace $ Maybe.catMaybes [goUp,goRight,goDown,goLeft]
  where
    goUp    = if y-1 < 0 then Nothing else Just (x,y-1)
    goRight = Just (x+1,y)
    goDown  = Just (x,y+1)
    goLeft  = if x-1 < 0 then Nothing else Just (x-1,y)

-- 2. Work our way through to find a solution
--

data Move = Move { movesMade :: [(Int,Int)], moveSteps :: Int }
    deriving Show

nextPositions' :: [(Int,Int)] -> [(Int,Int)]
nextPositions' (xy:rest) = filter notInRest $ nextPositions xy
  where
    restSet = Set.fromList rest
    notInRest pos = not $ Set.member pos $ restSet

step :: [Move] -> (Int,Int) -> [Move]
step moves final = if List.null moves then [] else newMoves
  where
    newMoves = bestScoreFirst (nextMoves ++ tail bestMoves)
      where
        bestMoves = bestScoreFirst moves
        bestMove = head bestMoves
        toMove xy = Move (xy : movesMade bestMove) (moveSteps bestMove + 1)
        nextMoves = fmap toMove $ nextPositions' $ movesMade $ bestMove
    bestScoreFirst moves = List.sortBy (comparing score) moves
    score move = moveSteps move + distance (head $ movesMade move) final
    distance (x1,y1) (x2,y2) = abs (x2 - x1) + abs (y2 - y1)

search :: (Int,Int) -> (Int,Int) -> Move
search startPos finishPos =
  let
    loop moves = case findFinishedMove moves of
        Nothing -> loop $ step moves finishPos
        Just move -> move
    findFinishedMove moves = case moves of
        [] -> Nothing
        (m:ms) -> if head (movesMade m) == finishPos then Just m else findFinishedMove ms
  in
    loop [Move [startPos] 0]

-- 3. for star 2, find the number of (x,y) coords at most 50 steps away:
--

findCoordsInMoves :: (Int,Int) -> Int -> Int
findCoordsInMoves startPos maxDistance = 
  let
    dedupe as = doDedupe as Set.empty
      where 
        doDedupe [] s = []
        doDedupe (a:as) s = 
          if Set.member a s then as else a : doDedupe as (Set.insert a s)  
    loop n positions coords = if n == maxDistance
        then coords
        else 
            let newMs = filter (\p -> not $ Set.member p coords) $ dedupe $ List.concat $ fmap nextPositions positions 
            in loop (n+1) newMs (Set.union coords (Set.fromList newMs)) 
  in
    Set.size $ loop 0 [startPos] Set.empty

main :: IO ()
main = do
    
    putStrLn "Star 1:"
    print $ moveSteps $ search (1,1) (31,39)

    putStrLn "Star 2:"
    print $ findCoordsInMoves (1,1) 50
