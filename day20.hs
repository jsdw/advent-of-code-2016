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
compactRanges rs = loop rs []
  where
    loop :: [Range] -> [Range] -> [Range]
    loop [] outRs = outRs
    loop (r:rs) outRs =
        let (newR,newRs) = compress r rs []
        in loop newRs (newR:outRs)
    compress :: Range -> [Range] -> [Range] -> (Range,[Range])
    compress r [] newRs = (r, newRs)
    compress (lo1,hi1) ((lo2,hi2):ss) newRs =
        if lo1 <= lo2 && hi1 >= lo2 then compress (lo1, max hi1 hi2) ss newRs
        else if lo2 <= lo1 && hi2 >= lo1 then compress (lo2, max hi1 hi2) ss newRs
        else compress (lo1,hi1) ss ((lo2,hi2):newRs)

isInRanges :: [Range] -> Word32 -> Bool
isInRanges rs w = List.any (\(lo,hi) -> w >= lo && w <= hi) rs

main :: IO ()
main = do

    ranges <- fmap compactRanges loadInput

    putStrLn "Star 1:"
    print $ head $ filter (not . isInRanges ranges) $ [0..]

    putStrLn "Star 2:"
    print $ length $ filter (not . isInRanges ranges) $ [0..maxBound]