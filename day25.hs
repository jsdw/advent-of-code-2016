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

{-

--- Day 25: Clock Signal ---

You open the door and find yourself on the roof. The city sprawls away from you for miles and miles.

There's not much time now - it's already Christmas, but you're nowhere near the North Pole, much too far to deliver these stars to the sleigh in time.

However, maybe the huge antenna up here can offer a solution. After all, the sleigh doesn't need the stars, exactly; it needs the timing data they provide, and you happen to have a massive signal generator right here.

You connect the stars you have to your prototype computer, connect that to the antenna, and begin the transmission.

Nothing happens.

You call the service number printed on the side of the antenna and quickly explain the situation. "I'm not sure what kind of equipment you have connected over there," he says, "but you need a clock signal." You try to explain that this is a signal for a clock.

"No, no, a clock signal - timing information so the antenna computer knows how to read the data you're sending it. An endless, alternating pattern of 0, 1, 0, 1, 0, 1, 0, 1, 0, 1...." He trails off.

You ask if the antenna can handle a clock signal at the frequency you would need to use for the data from the stars. "There's no way it can! The only antenna we've installed capable of that is on top of a top-secret Easter Bunny installation, and you're definitely not-" You hang up the phone.

You've extracted the antenna's clock signal generation assembunny code (your puzzle input); it looks mostly compatible with code you worked on just recently.

This antenna code, being a signal generator, uses one extra instruction:

out x transmits x (either an integer or the value of a register) as the next value for the clock signal.
The code takes a value (via register a) that describes the signal to generate, but you're not sure how it's used. You'll have to find the input to produce the right signal through experimentation.

What is the lowest positive integer that can be used to initialize register a and cause the code to output a clock signal of 0, 1, 0, 1... repeating forever?

-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.Attoparsec.Text (sepBy, endOfLine, Parser, signed, decimal, string, parseOnly, many1, letter)
import Data.Map.Strict (Map)
import Data.Vector (Vector, (!?), (//))
import Data.Text (Text)
import Control.Applicative ((<|>))
import Debug.Trace

-- 1. represent and parse instructions from input
--

type RegisterId = Text

data Location = Register RegisterId | Value Int
    deriving Show

data Instruction 
    = Cpy Location RegisterId -- copy thing at location to register
    | Inc RegisterId          -- increment value at register
    | Dec RegisterId          -- decrement value at register
    | Jump Location Location  -- if val at register is NOT 0, jump #instructions
    | Out Location            -- output the value at location
    deriving Show

instructionParser :: Parser [Instruction]
instructionParser = (cpyParser <|> incParser <|> decParser <|> jmpParser <|> outParser) `sepBy` endOfLine
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
    outParser = do
        string "out "
        loc <- locationParser
        return (Out loc)

loadInstructions :: IO Instructions
loadInstructions = do
    input <- Text.readFile "day25input.txt"
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
    , output :: Maybe Int
    } deriving Show

-- 3. run the world to completion
--

run :: World -> [World]
run w@(World instrs registers curr _) = w : case instrs !? curr of
    Just instr -> run (step instr)
    Nothing -> []
  where
    step instr = case instr of
        Cpy from to ->
            World instrs (Map.insert to (resolveValue from) registers) (curr+1) Nothing
        Inc registerId ->
            World instrs (Map.insertWith (+) registerId 1 registers) (curr+1) Nothing
        Dec registerId ->
            World instrs (Map.insertWith (+) registerId (-1) registers) (curr+1) Nothing
        Jump loc steps ->
            let (locVal, stepVal) = (resolveValue loc, resolveValue steps)
            in World instrs registers (if locVal /= 0 && stepVal /= 0 then curr + stepVal else curr + 1) Nothing
        Out loc ->
            World instrs registers (curr + 1) (Just $ resolveValue loc)
    resolveValue loc = case loc of
        Register id -> Map.findWithDefault 0 id registers
        Value i     -> i

-- 0,1,0,1,0,1...
alternatingBits :: [Int]
alternatingBits =
  let loop b = (if b then 1 else 0) : loop (not b)
  in loop False

outputFrom :: World -> [Int]
outputFrom world = Maybe.catMaybes $ fmap output $ run world

initWorld :: Instructions -> Int -> World
initWorld is n = World is (Map.singleton "a" n) 0 Nothing 

main :: IO ()
main = do
    
    instructions <- loadInstructions

    let n = 10 -- how many bits do we compare

    putStrLn "Star 1:"
    print $ fst
          $ head
          $ filter (flip List.isPrefixOf alternatingBits . take n . snd)
          $ zip [0..]
          $ fmap (outputFrom . initWorld instructions) [0..]

  
