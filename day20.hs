#!/usr/bin/env stack
-- stack --resolver lts-7.13 --install-ghc runghc --package base --package text --package attoparsec -- -hide-all-packages

{-

--- Day 20: Firewall Rules ---

You'd like to set up a small hidden computer here so you can use it to get back into the network later. However, the corporate firewall only allows communication with certain external IP addresses.

You've retrieved the list of blocked IPs from the firewall, but the list seems to be messy and poorly maintained, and it's not clear which IPs are allowed. Also, rather than being written in dot-decimal notation, they are written as plain 32-bit integers, which can have any value from 0 through 4294967295, inclusive.

For example, suppose only the values 0 through 9 were valid, and that you retrieved the following blacklist:

5-8
0-2
4-7
The blacklist specifies ranges of IPs (inclusive of both the start and end value) that are not allowed. Then, the only IPs that this firewall allows are 3 and 9, since those are the only numbers not in any range.

Given the list of blocked IPs you retrieved from the firewall (your puzzle input), what is the lowest-valued IP that is not blocked?

--- Part Two ---

How many IPs are allowed by the blacklist?

-}

import qualified Data.Text.IO as Text
import qualified Data.List as List
import Data.Attoparsec.Text (char,decimal,sepBy,endOfLine,Parser,parseOnly)
import Data.Word (Word32)

type Range = (Word32,Word32)

inputParser :: Parser [Range]
inputParser = rangeParser `sepBy` endOfLine
  where
    rangeParser = do
      n1 <- decimal
      char '-'
      n2 <- decimal
      return (n1,n2)

loadInput :: IO [Range]
loadInput = do
    input <- Text.readFile "day20input.txt"
    return $ case parseOnly inputParser input of
        Left _ -> []
        Right ranges -> ranges

compactRanges :: [Range] -> [Range]
compactRanges [] = []
compactRanges rs = loop (List.sortOn fst rs)
  where
    loop [] = []
    loop (r:[]) = [r]
    loop ((a1,a2):(b1,b2):rs) =
      if b1 <= (a2+1) then loop ((a1, max a2 b2):rs) else (a1,a2) : loop ((b1,b2):rs)

getAllSafe :: [Range] -> [Word32]
getAllSafe rs = loop rs 0
  where
    loop [] n = [] -- should never happen
    loop ((lo,hi):rs) n = newRange ++ nextStep
      where
        newRange = if n == lo then [] else [n..lo-1]
        nextStep = if hi == maxBound then [] else loop rs (hi+1)

main :: IO ()
main = do

    safe <- fmap (getAllSafe . compactRanges) loadInput

    putStrLn "Star 1:"
    print $ head safe

    putStrLn "Star 2:"
    print $ length safe
