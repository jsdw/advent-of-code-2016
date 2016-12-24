#!/usr/bin/env stack
{- stack
   --resolver lts-7.13
   --install-ghc runghc
   --package base
   --package vector
   --package containers
   --
   -hide-all-packages
-}

{-# LANGUAGE TupleSections #-}

{-

--- Day 24: Air Duct Spelunking ---

You've finally met your match; the doors that provide access to the roof are locked tight, and all of the controls and related electronics are inaccessible. You simply can't reach them.

The robot that cleans the air ducts, however, can.

It's not a very fast little robot, but you reconfigure it to be able to interface with some of the exposed wires that have been routed through the HVAC system. If you can direct it to each of those locations, you should be able to bypass the security controls.

You extract the duct layout for this area from some blueprints you acquired and create a map with the relevant locations marked (your puzzle input). 0 is your current location, from which the cleaning robot embarks; the other numbers are (in no particular order) the locations the robot needs to visit at least once each. Walls are marked as #, and open passages are marked as .. Numbers behave like open passages.

For example, suppose you have a map like the following:

###########
#0.1.....2#
#.#######.#
#4.......3#
###########

To reach all of the points of interest as quickly as possible, you would have the robot take the following path:

0 to 4 (2 steps)
4 to 1 (4 steps; it can't move diagonally)
1 to 2 (6 steps)
2 to 3 (2 steps)
Since the robot isn't very fast, you need to find it the shortest route. This path is the fewest steps (in the above example, a total of 14) required to start at 0 and then visit every other location at least once.

Given your actual map, and starting from location 0, what is the fewest number of steps required to visit every non-0 number marked on the map at least once?

--- Part Two ---

Of course, if you leave the cleaning robot somewhere weird, someone is bound to notice.

What is the fewest number of steps required to start at 0, visit every non-0 number marked on the map at least once, and then return to 0?

-}

import qualified Data.Vector as Vector
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Map (Map)
import Data.Vector (Vector, (!))

-- 1. parse the input into a maze
--

data Block = Wall | Empty | Target Int deriving (Show, Eq)
type Maze = Vector (Vector Block)

at :: Maze -> (Int,Int) -> Block
at maze (x,y) = maze ! y ! x

toBlock :: Char -> Block
toBlock c =
  if c >= '0' && c <= '9' then Target (read [c])
  else if c == '#' then Wall
  else Empty

parseMaze :: String -> Maze
parseMaze str = Vector.fromList $ fmap (Vector.fromList . fmap toBlock) (lines str)

loadInput :: IO Maze
loadInput = do
    input <- readFile "day24input.txt"
    return $ parseMaze input

-- 2. traverse the maze from each Target to find distance from it to
--    the other targets.

targetLocations :: Maze -> [(Int,Int)]
targetLocations maze = Vector.ifoldl' oneRow [] maze
  where
    oneRow acc y row =
        Vector.ifoldl' (\acc x b -> if isTarget b then (x,y):acc else acc) acc row

isTarget :: Block -> Bool
isTarget (Target _) = True
isTarget _ = False

targetId :: Block -> Int
targetId (Target n) = n
targetId _ = -1

moves :: Maze -> (Int,Int) -> [(Int,Int)]
moves maze (x,y) = filter (\pos -> maze `at` pos /= Wall) [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

distanceToTargets :: Maze -> (Int,Int) -> Map Int Int
distanceToTargets maze pos = search [(pos,0)] Map.empty Set.empty
  where
    search [] locations seen = locations 
    search ((pos,dist):rest) locations seen =
        search (rest ++ next) (Map.union locations $ Map.fromList targets) (Set.union seen $ Set.fromList nextPositions)
      where
        nextPositions = filter (\m -> not $ Set.member m seen) $ moves maze pos
        targets = fmap (,dist+1) $ fmap targetId $ filter isTarget $ fmap (at maze) nextPositions
        next = fmap (,dist+1) nextPositions

allDistancesBetweenLocations :: Maze -> Map Int (Map Int Int)
allDistancesBetweenLocations maze = Map.fromList $ zip targetIds distances
  where
    locs = targetLocations maze
    distances = fmap (distanceToTargets maze) locs
    targetIds = fmap (targetId . at maze) locs

-- 3. now that we have a graph of locations and their distances to eachother,
--    it's pretty easy to work out the shortest path (given the small data set)

shortestPathBetweenLocations :: Int -> Maybe Int -> Map Int (Map Int Int) -> Int
shortestPathBetweenLocations start finish graph = minimum $ fmap pathDistance paths
  where
    paths = fmap attachFinish
        $ filter (\p -> head p == start)
        $ List.permutations
        $ Map.foldlWithKey (\ls l _ -> l:ls) [] graph
    attachFinish = case finish of
        Nothing -> id
        Just n -> (++ [n])
    pathDistance (a:b:rest) = case Map.lookup a graph >>= Map.lookup b of
        Nothing -> error "route not found"
        Just d  -> d + pathDistance (b:rest)
    pathDistance _ = 0 

-- 4. collect our stars!
--

main :: IO ()
main = do

    maze <-loadInput

    let graph = allDistancesBetweenLocations maze

    putStrLn "Star 1:"
    print $ shortestPathBetweenLocations 0 Nothing $ graph

    putStrLn "Star 2:"
    print $ shortestPathBetweenLocations 0 (Just 0) $ graph
