#!/usr/bin/env stack
-- stack --resolver lts-7.13 --install-ghc runghc --package containers

{-

--- Day 2: Bathroom Security ---

You arrive at Easter Bunny Headquarters under cover of darkness. However, you left in such a rush that you forgot to use the bathroom! Fancy office buildings like this one usually have keypad locks on their bathrooms, so you search the front desk for the code.

"In order to improve security," the document you find says, "bathroom codes will no longer be written down. Instead, please memorize and follow the procedure below to access the bathrooms."

The document goes on to explain that each button to be pressed can be found by starting on the previous button and moving to adjacent buttons on the keypad: U moves up, D moves down, L moves left, and R moves right. Each line of instructions corresponds to one button, starting at the previous button (or, for the first line, the "5" button); press whatever button you're on at the end of each line. If a move doesn't lead to a button, ignore it.

You can't hold it much longer, so you decide to figure out the code as you walk to the bathroom. You picture a keypad like this:

1 2 3
4 5 6
7 8 9
Suppose your instructions are:

ULL
RRDDD
LURDL
UUUUD
You start at "5" and move up (to "2"), left (to "1"), and left (you can't, and stay on "1"), so the first button is 1.
Starting from the previous button ("1"), you move right twice (to "3") and then down three times (stopping at "9" after two moves and ignoring the third), ending up with 9.
Continuing from "9", you move left, up, right, down, and left, ending with 8.
Finally, you move up four times (stopping at "2"), then down once, ending with 5.
So, in this example, the bathroom code is 1985.

Your puzzle input is the instructions from the document you found at the front desk. What is the bathroom code?

--- Part Two ---

You finally arrive at the bathroom (it's a several minute walk from the lobby so visitors can behold the many fancy conference rooms and water coolers on this floor) and go to punch in the code. Much to your bladder's dismay, the keypad is not at all like you imagined it. Instead, you are confronted with the result of hundreds of man-hours of bathroom-keypad-design meetings:

    1
  2 3 4
5 6 7 8 9
  A B C
    D
You still start at "5" and stop when you're at an edge, but given the same instructions as above, the outcome is very different:

You start at "5" and don't move at all (up and left are both edges), ending at 5.
Continuing from "5", you move right twice and down three times (through "6", "7", "B", "D", "D"), ending at D.
Then, from "D", you move five more times (through "D", "B", "C", "C", "B"), ending at B.
Finally, after five more moves, you end at 3.
So, given the actual keypad layout, the code would be 5DB3.

Using the same instructions in your puzzle input, what is the correct bathroom code?

-}

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
    cmds <- fmap parseCmds (readFile "day02input.txt")

    putStrLn "Star 1:"
    print $ star1 cmds

    putStrLn "Star 2:"
    print $ star2 cmds