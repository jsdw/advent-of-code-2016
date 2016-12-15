#!/usr/bin/env stack
-- stack --resolver lts-7.13 --install-ghc runghc --package attoparsec --package containers

import qualified Data.Text.IO as Text
import qualified Data.List as List
import Data.Attoparsec.Text as Parser

-- 1. Parse the input into tuples of triangle dimensions for star 1
--
parseTriangles1 :: Parser [(Int,Int,Int)]
parseTriangles1 = parseTriangle `sepBy` endOfLine
  where
    parseTriangle = do
        Parser.takeWhile isHorizontalSpace
        a <- decimal
        Parser.takeWhile1 isHorizontalSpace
        b <- decimal
        Parser.takeWhile1 isHorizontalSpace
        c <- decimal
        Parser.takeWhile isHorizontalSpace
        return (a,b,c)

-- 1.5. Parse the input a little differently for star 2
--
parseTriangles2 :: Parser [(Int,Int,Int)]
parseTriangles2 = fmap concat $ parseTriangleGroup `sepBy` endOfLine
  where
    parseRow = do
        Parser.takeWhile isHorizontalSpace
        a <- decimal
        Parser.takeWhile1 isHorizontalSpace
        b <- decimal
        Parser.takeWhile1 isHorizontalSpace
        c <- decimal
        Parser.takeWhile isHorizontalSpace
        return (a,b,c)
    parseTriangleGroup = do
        (a1,b1,c1) <- parseRow
        endOfLine
        (a2,b2,c2) <- parseRow
        endOfLine
        (a3,b3,c3) <- parseRow
        return [(a1,a2,a3), (b1,b2,b3), (c1,c2,c3)]

-- Given a filepath and a parser, give back a list of triangles:
loadTriangles :: FilePath -> Parser [(Int,Int,Int)] -> IO [(Int,Int,Int)]
loadTriangles name parser = do
    input <- Text.readFile name
    return (case parseOnly parser input of
        Left _ -> []
        Right triangles -> triangles)

-- 2. For a given tuple, is sum of any 2 sides larger than third?
-- the permutations function makes this super easy (tho isnt the
-- most efficient way to work it out)
--
sumOfSides (a,b,c) = all id $ fmap (\[a,b,c] -> a + b > c) $ List.permutations [a,b,c]

main = do

    -- Gold Star:
    putStrLn "Gold Star:"
    triangles <- loadTriangles "day3input.txt" parseTriangles1
    print $ length $ filter sumOfSides triangles

    -- Silver Star:
    putStrLn "Silver Star:"
    triangles <- loadTriangles "day3input.txt" parseTriangles2
    print $ length $ filter sumOfSides triangles
