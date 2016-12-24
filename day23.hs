#!/usr/bin/env stack
{- stack
   --resolver lts-7.13
   --install-ghc runghc
   --package base
   --package text
   --package attoparsec
   --package vector
   --package containers
   --
   -hide-all-packages
-}

{-# LANGUAGE OverloadedStrings #-}

{-

--- Day 23: Safe Cracking ---

This is one of the top floors of the nicest tower in EBHQ. The Easter Bunny's private office is here, complete with a safe hidden behind a painting, and who wouldn't hide a star in a safe behind a painting?

The safe has a digital screen and keypad for code entry. A sticky note attached to the safe has a password hint on it: "eggs". The painting is of a large rabbit coloring some eggs. You see 7.

When you go to type the code, though, nothing appears on the display; instead, the keypad comes apart in your hands, apparently having been smashed. Behind it is some kind of socket - one that matches a connector in your prototype computer! You pull apart the smashed keypad and extract the logic circuit, plug it into your computer, and plug your computer into the safe.

Now, you just need to figure out what output the keypad would have sent to the safe. You extract the assembunny code from the logic chip (your puzzle input).
The code looks like it uses almost the same architecture and instruction set that the monorail computer used! You should be able to use the same assembunny interpreter for this as you did there, but with one new instruction:

tgl x toggles the instruction x away (pointing at instructions like jnz does: positive means forward; negative means backward):

For one-argument instructions, inc becomes dec, and all other one-argument instructions become inc.
For two-argument instructions, jnz becomes cpy, and all other two-instructions become jnz.
The arguments of a toggled instruction are not affected.
If an attempt is made to toggle an instruction outside the program, nothing happens.
If toggling produces an invalid instruction (like cpy 1 2) and an attempt is later made to execute that instruction, skip it instead.
If tgl toggles itself (for example, if a is 0, tgl a would target itself and become inc a), the resulting instruction is not executed until the next time it is reached.
For example, given this program:

cpy 2 a
tgl a
tgl a
tgl a
cpy 1 a
dec a
dec a
cpy 2 a initializes register a to 2.
The first tgl a toggles an instruction a (2) away from it, which changes the third tgl a into inc a.
The second tgl a also modifies an instruction 2 away from it, which changes the cpy 1 a into jnz 1 a.
The fourth line, which is now inc a, increments a to 3.
Finally, the fifth line, which is now jnz 1 a, jumps a (3) instructions ahead, skipping the dec a instructions.
In this example, the final value in register a is 3.

The rest of the electronics seem to place the keypad entry (the number of eggs, 7) in register a, run the code, and then send the value left in register a to the safe.

What value should be sent to the safe?

--- Part Two ---

The safe doesn't open, but it does make several angry noises to express its frustration.

You're quite sure your logic is working correctly, so the only other thing is... you check the painting again. As it turns out, colored eggs are still eggs. Now you count 12.

As you run the program with this new input, the prototype computer begins to overheat. You wonder what's taking so long, and whether the lack of any instruction more powerful than "add one" has anything to do with it. Don't bunnies usually multiply?

Anyway, what value should actually be sent to the safe?

-}

import Data.Attoparsec.Text as Parser
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Vector (Vector, (!?), (//))
import Data.Text (Text)
import Control.Applicative ((<|>))

-- 1. represent and parse instructions from input
--

type RegisterId = Text

data Location = Register RegisterId | Value Int
    deriving Show

data Instruction 
    = Cpy Location RegisterId    -- copy thing at location to register
    | Inc RegisterId             -- increment value at register
    | Dec RegisterId             -- decrement value at register
    | Jump Location Location     -- if val at register is NOT 0, jump #instructions
    | Tgl RegisterId             -- alter instructions +/- steps away
    | SkipJmp Location Location  -- running Tgl on a Jump might be invalid, so mark as skipped
    deriving Show

instructionParser :: Parser [Instruction]
instructionParser = (cpyParser <|> incParser <|> decParser <|> jmpParser <|> tglParser) `sepBy` endOfLine
  where
    number =
        signed decimal
    registerParser = do
        chars <- many1 letter
        return (Text.pack chars)
    locationParser =
        (registerParser >>= return . Register) <|> (number >>= return . Value)
    cpyParser = do
        string "cpy "
        loc <- locationParser
        string " "
        reg <- registerParser
        return (Cpy loc reg)
    incParser = do
        string "inc "
        reg <- registerParser
        return (Inc reg)
    decParser = do
        string "dec "
        reg <- registerParser
        return (Dec reg)
    jmpParser = do
        string "jnz "
        reg <- locationParser
        string " "
        steps <- locationParser
        return (Jump reg steps)
    tglParser = do
        string "tgl "
        reg <- registerParser
        return (Tgl reg)

loadInstructions :: IO Instructions
loadInstructions = do
    input <- Text.readFile "day23input.txt"
    return $ case parseOnly instructionParser input of
        Left err -> error err
        Right instrs -> Vector.fromList instrs

-- 2. represent the world in which these instructions are applied
--

type Registers = Map RegisterId Int
type Instructions = Vector Instruction

data World = World 
    { instructions :: Instructions
    , registers :: Registers
    , current :: Int
    } deriving Show

-- 3. run the world to completion
--

run :: World -> [World]
run w@(World instrs registers curr) = w : case instrs !? curr of
    Just instr -> run (step instr)
    Nothing -> []
  where
    step instr = case instr of
        Cpy from to ->
            World instrs (Map.insert to (resolveValue from) registers) (curr+1)
        Inc registerId ->
            World instrs (Map.insertWith (+) registerId 1 registers) (curr+1)
        Dec registerId ->
            World instrs (Map.insertWith (+) registerId (-1) registers) (curr+1)
        Jump loc steps ->
            let (locVal, stepVal) = (resolveValue loc, resolveValue steps)
            in World instrs registers (if locVal /= 0 && stepVal /= 0 then curr + stepVal else (curr+1))
        Tgl reg ->
            let idx = curr + Map.findWithDefault 0 reg registers
            in World (toggleInstructionAt idx instrs) registers (curr+1)
        SkipJmp _ _ ->
            World instrs registers (curr+1)
    resolveValue loc = case loc of
        Register id -> Map.findWithDefault 0 id registers
        Value i     -> i

toggleInstructionAt :: Int -> Instructions -> Instructions
toggleInstructionAt idx instrs = case instrs !? idx of
    Nothing -> instrs
    Just instr -> instrs // [(idx, toggle instr)]

toggle :: Instruction -> Instruction
toggle instr = case instr of
    Cpy from to ->
        Jump from (Register to)
    Inc reg ->
        Dec reg
    Dec reg ->
        Inc reg
    Jump loc steps ->
        case steps of
            Register registerId -> Cpy loc registerId
            Value _ -> SkipJmp loc steps
    Tgl reg ->
        Inc reg
    SkipJmp loc steps ->
        Jump loc steps

main :: IO ()
main = do
    
    instructions <- loadInstructions
    let res n = Map.lookup "a" $ registers $ last $ run $ World instructions (Map.singleton "a" n) 0

    putStrLn "Star 1:"
    print $ res 7

    -- this takes a number of minutes to complete (compiled):
    putStrLn "Star 2:"
    print $ res 12



