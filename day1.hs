#!/usr/bin/env stack
-- stack --resolver lts-7.13 --install-ghc runghc --package attoparsec --package containers

{-

--- Day 1: No Time for a Taxicab ---

Santa's sleigh uses a very high-precision clock to guide its movements, and the clock's oscillator is regulated by stars. Unfortunately, the stars have been stolen... by the Easter Bunny. To save Christmas, Santa needs you to retrieve all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each day in the advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!

You're airdropped near Easter Bunny Headquarters in a city somewhere. "Near", unfortunately, is as close as you can get - the instructions on the Easter Bunny Recruiting Document the Elves intercepted start here, and nobody had time to work them out further.

The Document indicates that you should start at the given coordinates (where you just landed) and face North. Then, follow the provided sequence: either turn left (L) or right (R) 90 degrees, then walk forward the given number of blocks, ending at a new intersection.

There's no time to follow such ridiculous instructions on foot, though, so you take a moment and work out the destination. Given that you can only walk on the street grid of the city, how far is the shortest path to the destination?

For example:

Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away.
R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away.
R5, L5, R5, R3 leaves you 12 blocks away.
How many blocks away is Easter Bunny HQ?

--- Part Two ---

Then, you notice the instructions continue on the back of the Recruiting Document. Easter Bunny HQ is actually at the first location you visit twice.

For example, if your instructions are R8, R4, R4, R8, the first location you visit twice is 4 blocks away, due East.

How many blocks away is the first location you visit twice?

-}

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Set as Set
import Control.Applicative ((<|>))
import Data.Attoparsec.Text

-- 1. parse the commands string into a list of Commands
--
type Steps = Int
data Command = L | R | F Steps deriving (Show)

parseCommands :: Parser [Command]
parseCommands =
  let
    leftCmd    = char 'L' >> return L
    rightCmd   = char 'R' >> return R
    forwardCmd = decimal >>= return . F
    cruft = many' (char ',' <|> char ' ')
  in
    many' ((leftCmd <|> rightCmd <|> forwardCmd) <* cruft)

toCommands :: Text.Text -> [Command]
toCommands input = case parseOnly parseCommands input of
    Left err -> error err
    Right cmds -> cmds

-- 2. convert the list of commands into a list of coords (we
-- do this partly because silver star requires the whole list,
-- otherwise we'd only keep the last ones)
--
data Direction = North | East | South | West deriving (Show,Enum)

turn :: Int -> Direction -> Direction
turn i dir =
  let
    n = (fromEnum dir + i) `mod` 4
  in
    toEnum (if n < 0 then 4 - n else n)

toCoords :: [Command] -> [(Int,Int)]
toCoords cmds =
  let
    next dir x y cmd = case cmd of
        R -> (turn 1 dir, x, y)
        L -> (turn (-1) dir, x, y)
        F steps -> case dir of
            North -> (dir, x, y + steps)
            East  -> (dir, x + steps, y)
            South -> (dir, x, y - steps)
            West  -> (dir, x - steps, y)
    applyCmd (dir, coords@((x,y):_)) cmd =
        let (newDir, newX, newY) = next dir x y cmd
        in if (newX,newY) == (x,y) then (newDir,coords) else (newDir, (newX,newY):coords)
    (finalDir, coords) =
        List.foldl' applyCmd (North, [(0, 0)]) cmds
  in
    coords

-- 2.5. for the silver star, collapse a list of coords given (newest at front)
-- into the first coords we see that are passed through twice
--
coordPath :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
coordPath (x1,y1) (x2,y2) =
  let
    minMax a b = if a < b then (a,b,id) else (b,a,reverse)
  in
    if y1 == y2 then
        let (low,high,ap) = minMax x1 x2 in ap $ fmap (\x -> (x,y1)) [low..high]
    else if x1 == x2 then
        let (low,high,ap) = minMax y1 y2 in ap $ fmap (\y -> (x1,y)) [low..high]
    else
        error "only one dimension should change at a time"

-- take in coords we've been to, newest at the front. use coordpath to
-- give us a path between each coord and the next one. Add the path (
-- careful to not dupe the current coord) to a set and return the first coord
-- in the path that we've already visited.
visitedTwice :: [(Int,Int)] -> (Int,Int)
visitedTwice coords =
  let
    firstMemberIn set path =
        List.foldl' (\(m,set) xy ->
            if m /= Nothing then (m,set)
            else if Set.member xy set then (Just xy, set)
            else (m, Set.insert xy set)) (Nothing,set) path
    run set [] = (0,0)
    run set [coord] = coord
    run set (coord:nextCoord:coords) =
        let (member,newSet) = firstMemberIn set (tail $ coordPath coord nextCoord)
        in case member of
            Nothing -> run newSet (nextCoord:coords)
            Just coord -> coord
  in
    run Set.empty (reverse coords)

-- 3. get the distance from the coords
--
toDistance (x,y) = abs x + abs y

-- 4. print out the results!
--
main = do

    input <- Text.readFile "day1input.txt"

    putStrLn "Star 1:"
    print $ toDistance $ head $ toCoords $ toCommands input

    putStrLn "Star 2:"
    print $ toDistance $ visitedTwice $ toCoords $ toCommands input

