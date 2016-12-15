#!/usr/bin/env stack
-- stack --resolver lts-7.13 --install-ghc runghc --package containers

import qualified Data.Map as Map
import qualified Data.List as List

-- 1. Generate keypad transitions. Easy to manually make for star 1
-- but harder for star 2, so automate it.
--
data Key = NoKey | Key Char deriving (Show)
type Keypad = [[Key]]

keyToChar :: Key -> Char -> Char
keyToChar key def = case key of
    Key i -> i
    NoKey -> def

genTransitions :: Keypad -> [(Char,(Char,Char,Char,Char))]
genTransitions pad =
  let
    h = length pad
    w = length (pad !! 0)
    coords = [(x,y) | x <- [0..w-1], y <- [0..h-1]]
    getTransitions (x,y) =
      let
        curr  = keyToChar (pad !! y !! x) '-'
        up    = pad !! (if y-1 < 0 then y else y-1)  !! x
        down  = pad !! (if y+1 >= h then y else y+1) !! x
        left  = pad !! y !! (if x-1 < 0 then x else x-1)
        right = pad !! y !! (if x+1 >= w then x else x+1)
      in
        (curr,
            (keyToChar up curr
            ,keyToChar right curr
            ,keyToChar down curr
            ,keyToChar left curr))
  in
    fmap getTransitions coords

-- 2. Parse a set of string commands into something we can work with
--
data Cmd = U | R | D | L deriving (Show)

parseCmds :: String -> [[Cmd]]
parseCmds str =
  let
    toCmd c = case c of
        'U' -> U
        'R' -> R
        'D' -> D
        'L' -> L
        _ -> error "command not recognised"
    parseLine line = case line of
        [] -> []
        (c:rest) -> toCmd c : parseLine rest
  in
    fmap parseLine (lines str)

-- 3. Given a keypad, command and starting key, figure out what the next key will be
--
runCmd :: Keypad -> Cmd -> Char -> Char
runCmd pad cmd n =
  let
    transitions =
        genTransitions pad
    pick cmd (a,b,c,d) = case cmd of
        U -> a
        R -> b
        D -> c
        L -> d
    tm = Map.fromList transitions
  in
    case Map.lookup n tm of
        Nothing -> error ("invalid starting n of "++show n)
        Just opts -> pick cmd opts

runCmds :: Keypad -> [Cmd] -> Char -> Char
runCmds pad cmds n = case cmds of
    [] -> n
    (cmd:rest) -> let n' = runCmd pad cmd n in runCmds pad rest n'

runLines :: Keypad -> [[Cmd]] -> Char -> [Char]
runLines pad lines n = case lines of
    [] -> []
    (line:rest) -> let n' = runCmds pad line n in n' : runLines pad rest n'

-- 4. Run each of the puzzles given their respective keypads
-- and print the results
--
main =
  let

    -- part 1:
    star1 cmds =
      let
        pad = [ [ Key '1', Key '2', Key '3' ]
              , [ Key '4', Key '5', Key '6' ]
              , [ Key '7', Key '8', Key '9' ] ]
      in
        runLines pad cmds '5'

    -- part 2:
    star2 cmds =
      let
        pad = [ [ NoKey,   NoKey,   Key '1', NoKey,   NoKey   ]
              , [ NoKey,   Key '2', Key '3', Key '4', NoKey   ]
              , [ Key '5', Key '6', Key '7', Key '8', Key '9' ]
              , [ NoKey,   Key 'A', Key 'B', Key 'C', NoKey   ]
              , [ NoKey,   NoKey,   Key 'D', NoKey,   NoKey   ] ]
      in
        runLines pad cmds '5'

  in do
    cmds <- fmap parseCmds (readFile "day2input.txt")

    putStrLn "Gold:"
    print $ star1 cmds

    putStrLn "Silver:"
    print $ star2 cmds