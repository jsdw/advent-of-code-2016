#!/usr/bin/env stack
-- stack --resolver lts-7.13 --install-ghc runghc --package base --package text --package attoparsec -- -hide-all-packages

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
