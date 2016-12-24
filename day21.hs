#!/usr/bin/env stack
-- stack --resolver lts-7.13 --install-ghc runghc --package base --package vector --package text --package attoparsec -- -hide-all-packages

{-# LANGUAGE OverloadedStrings #-}

{-

--- Day 21: Scrambled Letters and Hash ---

The computer system you're breaking into uses a weird scrambling function to store its passwords. It shouldn't be much trouble to create your own scrambled password so you can add it to the system; you just have to implement the scrambler.

The scrambling function is a series of operations (the exact list is provided in your puzzle input). Starting with the password to be scrambled, apply each operation in succession to the string. The individual operations behave as follows:

swap position X with position Y means that the letters at indexes X and Y (counting from 0) should be swapped.
swap letter X with letter Y means that the letters X and Y should be swapped (regardless of where they appear in the string).
rotate left/right X steps means that the whole string should be rotated; for example, one right rotation would turn abcd into dabc.
rotate based on position of letter X means that the whole string should be rotated to the right based on the index of letter X (counting from 0) as determined before this instruction does any rotations. Once the index is determined, rotate the string to the right one time, plus a number of times equal to that index, plus one additional time if the index was at least 4.
reverse positions X through Y means that the span of letters at indexes X through Y (including the letters at X and Y) should be reversed in order.
move position X to position Y means that the letter which is at index X should be removed from the string, then inserted such that it ends up at index Y.
For example, suppose you start with abcde and perform the following operations:

swap position 4 with position 0 swaps the first and last letters, producing the input for the next step, ebcda.
swap letter d with letter b swaps the positions of d and b: edcba.
reverse positions 0 through 4 causes the entire string to be reversed, producing abcde.
rotate left 1 step shifts all letters left one position, causing the first letter to wrap to the end of the string: bcdea.
move position 1 to position 4 removes the letter at position 1 (c), then inserts it at position 4 (the end of the string): bdeac.
move position 3 to position 0 removes the letter at position 3 (a), then inserts it at position 0 (the front of the string): abdec.
rotate based on position of letter b finds the index of letter b (1), then rotates the string right once plus a number of times equal to that index (2): ecabd.
rotate based on position of letter d finds the index of letter d (4), then rotates the string right once, plus a number of times equal to that index, plus an additional time because the index was at least 4, for a total of 6 right rotations: decab.
After these steps, the resulting scrambled password is decab.

Now, you just need to generate a new scrambled password and you can access the system. Given the list of scrambling operations in your puzzle input, what is the result of scrambling abcdefgh?

--- Part Two ---

You scrambled the password correctly, but you discover that you can't actually modify the password file on the system. You'll need to un-scramble one of the existing passwords by reversing the scrambling process.

What is the un-scrambled version of the scrambled password fbgdceah?

-}

import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector
import qualified Data.List as List
import Data.Attoparsec.Text
import Data.Vector (Vector, (//), (!))
import Data.Monoid ((<>))
import Control.Monad (guard)
import Control.Applicative ((<|>))

data Cmd
  = SwapPosition Int Int  --swap letters at index Int and Int
  | SwapLetters Char Char --swap letters Char and Char
  | RotateLeft Int        --rotate all letters left by Int
  | RotateRight Int       --rotate all letters right by Int
  | RotateBasedOn Char    --rotate right based on index of Char
  | Reverse Int Int       --reverse all letters between index Int and Int (inclusive)
  | Move Int Int          --move letter from index Int to new index Int
  deriving Show

commandParser :: Parser [Cmd]
commandParser = cmd `sepBy1` endOfLine
  where
    cmd = swapPosition
      <|> swapLetters
      <|> rotateLeft
      <|> rotateRight
      <|> rotateBasedOn
      <|> reverse
      <|> move
    swapPosition = do
      string "swap position "
      x <- decimal
      string " with position "
      y <- decimal
      return $ SwapPosition x y
    swapLetters = do
      string "swap letter "
      x <- letter
      string " with letter "
      y <- letter
      return $ SwapLetters x y
    rotateLeft = do
      (isLeft, steps) <- rotate
      guard isLeft
      return $ RotateLeft steps
    rotateRight = do
      (isLeft, steps) <- rotate
      guard (not isLeft)
      return $ RotateRight steps
    rotate = do
      string "rotate "
      dir <- string "left" <|> string "right"
      string " "
      steps <- decimal
      string " step"
      string "s" <|> string ""
      return (if dir == "left" then True else False, steps)
    rotateBasedOn = do
      string "rotate based on position of letter "
      x <- letter
      return $ RotateBasedOn x
    reverse = do
      string "reverse positions "
      x <- decimal
      string " through "
      y <- decimal
      return $ Reverse x y
    move = do
      string "move position "
      x <- decimal
      string " to position "
      y <- decimal
      return $ Move x y

loadCommands :: IO [Cmd]
loadCommands = do
  input <- Text.readFile "day21input.txt"
  return $ case parseOnly commandParser input of
    Left err -> error err
    Right cmds -> cmds

runCommand :: Cmd -> Vector Char -> Vector Char
runCommand cmd txt = case cmd of
    SwapPosition x y ->
        txt // [(x, txt ! y), (y, txt ! x)]
    SwapLetters x y ->
        case (Vector.findIndex (== x) txt, Vector.findIndex (== y) txt) of
            (Just xIdx, Just yIdx) -> txt // [(xIdx, txt ! yIdx), (yIdx, txt ! xIdx)]
            _ -> txt
    RotateRight x ->
        rotateRight x
    RotateLeft x ->
        rotateRight (len - (x `mod` len))
    RotateBasedOn c ->
        case Vector.findIndex (== c) txt of
            Just idx -> rotateRight (1 + idx + if idx >= 4 then 1 else 0)
            Nothing -> txt
    Reverse x' y' ->
        let (x,y) = (min x' y', max x' y')
        in txt // (zip [x..y] $ reverse $ fmap (txt !) [x..y])
    Move x y ->
        let txt' = Vector.ifilter (\idx _ -> idx /= x) txt
        in Vector.take y txt' <> Vector.singleton (txt ! x) <> Vector.drop y txt'
  where
    len =
        Vector.length txt
    rotateRight x =
        txt // (zip (fmap (\i -> (i+x) `mod` len) [0..]) $ Vector.toList txt)
    
runReversedCommand :: Cmd -> Vector Char -> Vector Char
runReversedCommand cmd txt = case cmd of
    RotateRight x ->
        runCommand (RotateLeft x) txt
    RotateLeft x ->
        runCommand (RotateRight x) txt
    RotateBasedOn c ->
        runCommand (RotateRight $ reverseRotateNBasedOn c) txt
    Move x y ->
        runCommand (Move y x) txt
    cmd ->
        runCommand cmd txt
  where
    rotationsToReverseBasedOn =
        Vector.fromList [7,7,2,6,1,5,0,4] -- manually reverse index shifting of rotateBasedOn
    reverseRotateNBasedOn c =
        case Vector.findIndex (== c) txt of
            Nothing  -> 0
            Just idx -> rotationsToReverseBasedOn ! idx

runCommands cmds pw = List.foldl' (flip runCommand) pw cmds
runCommandsReversed cmds pw = List.foldr runReversedCommand pw cmds

main :: IO ()
main = do

    cmds <-loadCommands

    putStrLn "Star 1:"
    print $ runCommands cmds (Vector.fromList "abcdefgh")

    putStrLn "Star 2:"
    print $ runCommandsReversed cmds (Vector.fromList "fbgdceah") 
