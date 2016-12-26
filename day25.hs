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

  
