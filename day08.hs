#!/usr/bin/env stack
-- stack --resolver lts-7.13 --install-ghc runghc --package attoparsec --package containers

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

{-

--- Day 8: Two-Factor Authentication ---

You come across a door implementing what you can only assume is an implementation of two-factor authentication after a long game of requirements telephone.

To get past the door, you first swipe a keycard (no problem; there was one on a nearby desk). Then, it displays a code on a little screen, and you type that code on a keypad. Then, presumably, the door unlocks.

Unfortunately, the screen has been smashed. After a few minutes, you've taken everything apart and figured out how it works. Now you just have to work out what the screen would have displayed.

The magnetic strip on the card you swiped encodes a series of instructions for the screen; these instructions are your puzzle input. The screen is 50 pixels wide and 6 pixels tall, all of which start off, and is capable of three somewhat peculiar operations:

rect AxB turns on all of the pixels in a rectangle at the top-left of the screen which is A wide and B tall.
rotate row y=A by B shifts all of the pixels in row A (0 is the top row) right by B pixels. Pixels that would fall off the right end appear at the left end of the row.
rotate column x=A by B shifts all of the pixels in column A (0 is the left column) down by B pixels. Pixels that would fall off the bottom appear at the top of the column.
For example, here is a simple sequence on a smaller screen:

rect 3x2 creates a small rectangle in the top-left corner:

###....
###....
.......
rotate column x=1 by 1 rotates the second column down by one pixel:

#.#....
###....
.#.....
rotate row y=0 by 4 rotates the top row right by four pixels:

....#.#
###....
.#.....
rotate column x=1 by 1 again rotates the second column down by one pixel, causing the bottom pixel to wrap back to the top:

.#..#.#
#.#....
.#.....
As you can see, this display technology is extremely powerful, and will soon dominate the tiny-code-displaying-screen market. That's what the advertisement on the back of the display tries to convince you, anyway.

There seems to be an intermediate check of the voltage used by the display: after you swipe your card, if the screen did work, how many pixels should be lit?

--- Part Two ---

You notice that the screen is only capable of displaying capital letters; in the font it uses, each letter is 5 pixels wide and 6 tall.

After you swipe your card, what code is the screen trying to display?

-}

import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector
import qualified Data.List as List
import Data.Attoparsec.Text as Parser
import Control.Applicative ((<|>))
import Control.Monad (forM_, foldM_)
import Data.Vector (Vector, (//), (!))

import Debug.Trace

-- 1. Parse the input file into a series of commands
--

data Cmd
    = Rect     Int Int -- width, height
    | ShiftRow Int Int -- row, shift right amount
    | ShiftCol Int Int -- col, shift down amount
    deriving (Show)

commandParser :: Parser [Cmd]
commandParser = (rectParser <|> shiftRowParser <|> shiftColParser) `sepBy` endOfLine
  where
    space = Parser.takeWhile1 isHorizontalSpace
    rectParser = do
        string "rect"
        space
        width <- decimal
        char 'x'
        height <- decimal
        return (Rect width height)
    shiftRowParser = do
        string "rotate row y="
        row <- decimal
        space
        string "by"
        space
        shift <- decimal
        return (ShiftRow row shift)
    shiftColParser = do
        string "rotate column x="
        col <- decimal
        space
        string "by"
        space
        shift <- decimal
        return (ShiftCol col shift)

loadInput :: IO [Cmd]
loadInput = do
    input <- Text.readFile "day08input.txt"
    return $ case parseOnly commandParser input of
        Left _ -> []
        Right cmds -> cmds

-- 2. create a screen
--

data Screen = Screen
    { screenPx :: Vector Bool
    , screenWidth :: Int
    , screenHeight :: Int }
    deriving (Show)

-- An empty screen of width and height to get us going:
emptyScreen :: Int -> Int -> Screen
emptyScreen w h = Screen (Vector.replicate (w*h) False) w h

-- Prints the current screen in ascii:
printScreen :: Screen -> IO ()
printScreen Screen{..} =
  let
    printHead = do
        putStr "/"
        forM_ [ 0 .. screenWidth - 1 ] $ \_ -> putStr "-"
        putStrLn "\\"
    printRow n = do
        let idxs = [ screenWidth * n .. screenWidth * n + screenWidth - 1 ]
        putStr "|"
        forM_ idxs $ \idx -> putStr $ if screenPx ! idx then "#" else " "
        putStrLn "|"
    printFoot = do
        putStr "\\"
        forM_ [ 0 .. screenWidth - 1] $ \_ -> putStr "-"
        putStrLn "/"
  in do
      printHead
      forM_ [ 0 .. screenHeight - 1 ] printRow
      printFoot

-- 3. decide on our drawing operations to transform the screen state
--

drawRect :: Int -> Int -> Screen -> Screen
drawRect x y screen@Screen{..} =
  let idxs = [ screenWidth * y' + x' | x' <- [0..x-1], y' <- [0..y-1] ]
  in screen { screenPx = screenPx // fmap (\idx -> (idx,True)) idxs }

rotateVec :: Int -> Vector a -> Vector a
rotateVec shiftBy vec =
  let
    prevIdx idx =
        let i = idx - (shiftBy `mod` Vector.length vec)
        in if i < 0 then Vector.length vec + i else i
  in
    Vector.imap (\idx a -> vec ! prevIdx idx) vec

drawShifted :: [Int] -> Int -> Vector a -> Vector a
drawShifted idxs shiftBy vec =
  let
    idxs' = Vector.fromList idxs
    newIdxs = rotateVec shiftBy idxs'
    newValues = Vector.imap (\i idx -> (idx,vec ! (newIdxs ! i)) ) idxs'
  in
    Vector.update vec newValues

drawShiftRow :: Int -> Int -> Screen -> Screen
drawShiftRow row shiftBy screen@Screen{..} =
    screen { screenPx = drawShifted [screenWidth * row..screenWidth * row + screenWidth - 1] shiftBy screenPx }

drawShiftCol :: Int -> Int -> Screen -> Screen
drawShiftCol col shiftBy screen@Screen{..} =
    screen { screenPx = drawShifted [ a * screenWidth + col | a <- [0..screenHeight-1] ] shiftBy screenPx }

-- 4. Update a screen given commands, by making use of the draw operations
--

updateScreen :: Cmd -> Screen -> Screen
updateScreen cmd screen = case cmd of
    Rect x y -> drawRect x y screen
    ShiftRow row shift -> drawShiftRow row shift screen
    ShiftCol col shift -> drawShiftCol col shift screen

main :: IO ()
main = do

    cmds <- loadInput

    putStrLn "Star 1:"
    let star1Screen = List.foldl' (\scr cmd -> updateScreen cmd scr) (emptyScreen 50 6) cmds
    print $ Vector.foldl' (\n v -> if v then n+1 else n) 0 (screenPx star1Screen)

    putStrLn "Star 2:"
    printScreen star1Screen
