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
      
      
