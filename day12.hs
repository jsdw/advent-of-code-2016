#!/usr/bin/env stack
-- stack --resolver lts-7.13 --install-ghc runghc --package attoparsec --package vector --package containers --package mtl

{-# LANGUAGE OverloadedStrings #-}

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
