#!/usr/bin/env stack
-- stack --resolver lts-7.13 --install-ghc runghc --package base --package attoparsec --package text --package vector --package containers -- -hide-all-packages

{-# LANGUAGE OverloadedStrings, BangPatterns #-}

import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Data.Vector (Vector, (//), (!))
import Data.Text (Text)
import Data.Word (Word8)
import Control.Applicative ((<|>))
import Data.Attoparsec.Text as Parser
import Data.Ord (comparing, Down(..))
import Debug.Trace

-- 1. describe the input and game state
--

data Piece = Generator Word8 | Microchip Word8 deriving (Show, Eq, Ord)
data State = State { statePieces :: Vector [Piece], stateFloor :: !Int } deriving Show

input1 = Vector.fromList
    [ [Generator 1, Generator 2, Microchip 2, Generator 3, Generator 4, Microchip 4, Generator 5, Microchip 5]
    , [Microchip 1, Microchip 3]
    , []
    , []
    ]

input2 = Vector.fromList
    [ (input1 ! 0) ++ [Generator 6, Generator 7, Microchip 6, Microchip 7]
    , input1 ! 1
    , input1 ! 2
    , input1 ! 3
    ]

-- 2. describe the rules of the game in terms of the moves it's possible to make from a given state.
--

isAllowedTogether :: [Piece] -> Bool
isAllowedTogether ps = Set.null gens || List.all (\c -> Set.member c gens) chips
  where
    (gens, chips) = groupByType ps Set.empty [] where
        groupByType ps gens chips = case ps of
            [] -> (gens,chips)
            (Generator n : rest) -> groupByType rest (Set.insert n gens) chips
            (Microchip n : rest) -> groupByType rest gens (n : chips)

moves :: State -> [State]
moves (State pieces currFloor) = Maybe.catMaybes $ fmap (updateStateWith (currFloor + 1)) movable ++ fmap (updateStateWith (currFloor - 1)) movable
  where
    currPieces = pieces ! currFloor
    movable = List.filter canBeMoved                                 -- make sure each combo of (leftover,tobemoved) is valid
        $ fmap (\ps -> (List.deleteFirstsBy (==) currPieces ps, ps)) -- find out what the leftover pieces would be and make tuple
        $ List.subsequences currPieces                               -- all of the different combinations of bits in the list that we might try and move
    canBeMoved (leftover,ps) =
        let l = List.length ps
        in l >= 1 && l <= 2 && isAllowedTogether ps && isAllowedTogether leftover
    updateStateWith newFloor (leftover,ps) =
        if newFloor < Vector.length pieces && newFloor >= 0 && isAllowedTogether newFloorPieces     -- would this new state be valid?
        then Just $ State (pieces // [(newFloor, newFloorPieces), (currFloor, leftover)]) newFloor  -- if so, make it!
        else Nothing
      where
        newFloorPieces = (pieces ! newFloor) ++ ps

-- 3. search through the possible moves to try and find the best solution
--

solve :: State -> Int
solve initialS = loop [initialS] (snd $ updateSeen Set.empty [initialS]) 1
  where
    loop states visited !turns =
      let (nextStates,nextVisited) = updateSeen visited $ List.concat $ fmap moves states
      in case List.find isSolution nextStates of
          Nothing -> loop nextStates nextVisited (turns+1)
          Just finalState -> turns
    updateSeen visited states =
        List.foldl' (\(!out, !seen) sh ->
          let stateIdentifier = uniqueIdentifier sh
          in if Set.member stateIdentifier seen
                then (out,seen)
                else (sh:out, Set.insert stateIdentifier seen)) ([],visited) states
    uniqueIdentifier (State pieces currFloor) =
        (fmap Set.fromList pieces, currFloor)

isSolution :: State -> Bool
isSolution (State pieces currFloor) =
    if currFloor == 3 && List.null (pieces ! 0) && List.null (pieces ! 1) && List.null (pieces ! 2) then True else False

main :: IO ()
main = do

    -- solve the input for star 1:
    putStrLn "Star 1:"
    print $ solve $ State input1 0

    -- slightly more input for star 2 (takes ~5.5mins compiled):
    putStrLn "Star 2:"
    print $ solve $ State input2 0