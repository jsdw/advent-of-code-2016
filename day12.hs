#!/usr/bin/env stack
-- stack --resolver lts-7.13 --install-ghc runghc --package attoparsec --package vector --package containers --package mtl

{-# LANGUAGE OverloadedStrings #-}

{-

--- Day 12: Leonardo's Monorail ---

You finally reach the top floor of this building: a garden with a slanted glass ceiling. Looks like there are no more stars to be had.

While sitting on a nearby bench amidst some tiger lilies, you manage to decrypt some of the files you extracted from the servers downstairs.

According to these documents, Easter Bunny HQ isn't just this building - it's a collection of buildings in the nearby area. They're all connected by a local monorail, and there's another building not far from here! Unfortunately, being night, the monorail is currently not operating.

You remotely connect to the monorail control systems and discover that the boot sequence expects a password. The password-checking logic (your puzzle input) is easy to extract, but the code it uses is strange: it's assembunny code designed for the new computer you just assembled. You'll have to execute the code and get the password.

The assembunny code you've extracted operates on four registers (a, b, c, and d) that start at 0 and can hold any integer. However, it seems to make use of only a few instructions:

cpy x y copies x (either an integer or the value of a register) into register y.
inc x increases the value of register x by one.
dec x decreases the value of register x by one.
jnz x y jumps to an instruction y away (positive means forward; negative means backward), but only if x is not zero.
The jnz instruction moves relative to itself: an offset of -1 would continue at the previous instruction, while an offset of 2 would skip over the next instruction.

For example:

cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a
The above code would set register a to 41, increase its value by 2, decrease its value by 1, and then skip the last dec a (because a is not zero, so the jnz a 2 skips it), leaving register a at 42. When you move past the last instruction, the program halts.

After executing the assembunny code in your puzzle input, what value is left in register a?

--- Part Two ---

As you head down the fire escape to the monorail, you notice it didn't start; register c needs to be initialized to the position of the ignition key.

If you instead initialize register c to be 1, what value is now left in register a?

-}

import Data.Attoparsec.Text as Parser
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Vector (Vector, (!?))
import Data.Text (Text)
import Control.Applicative ((<|>))

-- 1. represent and parse instructions from input
--

type RegisterId = Text

data Location = Register RegisterId | Value Int
    deriving Show

data Instruction 
    = Cpy Location RegisterId -- copy thing at location to register
    | Inc RegisterId          -- increment value at register
    | Dec RegisterId          -- decrement value at register
    | Jump Location Int       -- if val at register is NOT 0, jump #instructions
    deriving Show

instructionParser :: Parser [Instruction]
instructionParser = (cpyParser <|> incParser <|> decParser <|> jmpParser) `sepBy` endOfLine
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
        steps <- number
        return (Jump reg steps)

loadInstructions :: IO Instructions
loadInstructions = do
    input <- Text.readFile "day12input.txt"
    return $ case parseOnly instructionParser input of
        Left err -> error err
        Right instrs -> Vector.fromList instrs

-- 2. represent the world in which these instructions are applied
--

type Registers = Map RegisterId Int
type Instructions = Vector Instruction

data World = World 
    { registers :: Registers
    , current :: Int
    } deriving Show

-- 3. run the world to completion
--

run :: Instructions -> World -> World
run instrs (World registers curr) = case instrs !? curr of
    Just instr -> run instrs (step instr)
    Nothing -> World registers curr
  where
    step instr = case instr of
        Cpy from to ->
            World (Map.insert to (resolveValue from) registers) (curr+1)
        Inc registerId ->
            World (Map.insertWith (+) registerId 1 registers) (curr+1)
        Dec registerId ->
            World (Map.insertWith (+) registerId (-1) registers) (curr+1)
        Jump loc steps -> 
            World registers (if resolveValue loc /= 0 then curr + steps else (curr+1))  
    resolveValue loc = case loc of
        Register id -> Map.findWithDefault 0 id registers
        Value i     -> i

main :: IO ()
main = do
    
    instructions <- loadInstructions

    putStrLn "Star 1:"
    let world1 = World Map.empty 0
    print $ Map.lookup "a" $ registers $ run instructions world1

    putStrLn "Star 2:"
    let world2 = World (Map.singleton "c" 1) 0
    print $ Map.lookup "a" $ registers $ run instructions world2
