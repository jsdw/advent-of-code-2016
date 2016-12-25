#!/usr/bin/env stack
-- stack --resolver lts-7.13 --install-ghc runghc --package base --package vector --package text --package attoparsec -- -hide-all-packages

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as Text
import qualified Data.List as List
import Data.Attoparsec.Text

data Node = Node
    { nodeSize :: Int
    , nodeUsed :: Int
    , nodeAvail :: Int
    , nodePosition :: (Int,Int)
    } deriving (Show,Eq)

inputParser :: Parser [Node]
inputParser = do
    line
    line
    node `sepBy` endOfLine
  where
    line =
        skipWhile (not . isEndOfLine) >> endOfLine
    spaces =
        skipWhile isHorizontalSpace
    node = do
        string "/dev/grid/node-x"
        x <- decimal
        string "-y"
        y <- decimal
        spaces
        size <- sizeParser
        spaces
        used <- sizeParser
        spaces
        avail <- sizeParser
        skipWhile (not . isEndOfLine)
        return $ Node size used avail (x,y)
    sizeParser = do
        s <- decimal
        char 'T'
        return s

loadInputs :: IO [Node]
loadInputs = do
    input <- Text.readFile "day22input.txt"
    return $ case parseOnly inputParser input of
        Left err -> error err
        Right nodes -> nodes

countViablePairs :: [Node] -> Int
countViablePairs nodes =
    List.foldl' countPairsForNode 0 nodes
  where
    countPairsForNode acc n =
        List.foldl' (\acc' n' -> if isValidPair n n' then acc'+1 else acc') acc nodes
    isValidPair n1 n2 =
        n1 /= n2 && nodeUsed n1 > 0 && nodeUsed n1 <= nodeAvail n2

main :: IO ()
main = do

    nodes <- loadInputs

    putStrLn "Star 1:"
    print $ countViablePairs nodes

    putStrLn "Star 2:"
    -- did by hand by eyeballing the data; realisations:
    --
    -- 1. only place we can move data is into the single "free" space available
    -- 2. only place we cant "move" the free space is a "wall" (nodes that have
    --    too much data on to do anything with) running from (4,27) to (32,27)
    -- 3. free space is behind the wall at (15,29).
    --
    -- Thus,
    -- - we first use one move per square to move the free space around the wall
    --   and in front of the data (a total of 12+29+28 moves)
    -- - we use one move to swap data and free space
    -- - we then repeat until data is at (0,0) (ie 31 times) the steps:
    --   a) move free space around in front of the data again (4 moves)
    --   b) swap data with free space.
    print $ (12+29+28) + 1 + (1+4)*31
      
