#!/usr/bin/env stack
-- stack --resolver lts-7.13 --install-ghc runghc --package attoparsec --package mtl

{-# LANGUAGE OverloadedStrings #-}

{-
--- Day 10: Balance Bots ---

You come upon a factory in which many robots are zooming around handing small microchips to each other.

Upon closer examination, you notice that each bot only proceeds when it has two microchips, and once it does, it gives each one to a different bot or puts it in a marked "output" bin. Sometimes, bots take microchips from "input" bins, too.

Inspecting one of the microchips, it seems like they each contain a single number; the bots must use some logic to decide what to do with each chip. You access the local control computer and download the bots' instructions (your puzzle input).

Some of the instructions specify that a specific-valued microchip should be given to a specific bot; the rest of the instructions indicate what a given bot should do with its lower-value or higher-value chip.

For example, consider the following instructions:

value 5 goes to bot 2
bot 2 gives low to bot 1 and high to bot 0
value 3 goes to bot 1
bot 1 gives low to output 1 and high to bot 0
bot 0 gives low to output 2 and high to output 0
value 2 goes to bot 2
Initially, bot 1 starts with a value-3 chip, and bot 2 starts with a value-2 chip and a value-5 chip.
Because bot 2 has two microchips, it gives its lower one (2) to bot 1 and its higher one (5) to bot 0.
Then, bot 1 has two microchips; it puts the value-2 chip in output 1 and gives the value-3 chip to bot 0.
Finally, bot 0 has two microchips; it puts the 3 in output 2 and the 5 in output 0.
In the end, output bin 0 contains a value-5 microchip, output bin 1 contains a value-2 microchip, and output bin 2 contains a value-3 microchip. In this configuration, bot number 2 is responsible for comparing value-5 microchips with value-2 microchips.

Based on your instructions, what is the number of the bot that is responsible for comparing value-61 microchips with value-17 microchips?

--- Part Two ---

What do you get if you multiply together the values of one chip in each of outputs 0, 1, and 2?
-}

import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Control.Monad.State as State
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT)
import Control.Monad (guard, mzero)
import Control.Monad.Identity (Identity)
import Data.Map.Strict (Map)
import Data.Ord (Down(..),comparing)
import Data.Monoid ((<>))
import Control.Applicative ((<|>))
import Data.Attoparsec.Text as Parser

-- 1. parse input into commands
--

data Thing
    = Bot Int
    | Output Int
    deriving (Show)

data Cmd
    = ValueGoesTo Int Thing          -- give value of Int to Thing
    | BotGivesValues Int Thing Thing -- bot with id Int gives low and high to Thing and Thing
    deriving (Show)

commandParser :: Parser [Cmd]
commandParser = (valueGoesToParser <|> botGivesParser) `sepBy` endOfLine
  where
    thingParser = botThing <|> outputThing
      where
        botThing = do
            string "bot "
            Bot <$> decimal
        outputThing = do
            string "output "
            Output <$> decimal
    valueGoesToParser = do
        string "value "
        value <- decimal
        string " goes to "
        thing <- thingParser
        return $ ValueGoesTo value thing
    botGivesParser = do
        string "bot "
        botId <- decimal
        string " gives low to "
        low <- thingParser
        string " and high to "
        high <- thingParser
        return $ BotGivesValues botId low high

loadCmds :: IO [Cmd]
loadCmds = do
    input <- Text.readFile "day10input.txt"
    return $ case parseOnly commandParser input of
        Left _ -> []
        Right cmds -> cmds


-- 2. represent the state (bots with pairs of values, and output bins)
-- and operations on the state
--

type Bots = Map Int [Int]  -- a bot has an ID and maybe values
type Outputs = Map Int Int -- an output has a value (or doesnt exist yet if it doesnt)
data World = World { theBots :: Bots, theOutputs :: Outputs } deriving Show
type WorldState = State.StateT World Maybe

emptyWorld = World Map.empty Map.empty

giveValueToThing :: Int -> Thing -> World -> World
giveValueToThing val thing (World bots outputs) = case thing of
    Bot botId -> World newBots outputs
      where
        newBots = Map.alter alterFn botId bots
        alterFn Nothing = Just [val]
        alterFn (Just vals) = Just (val:vals)
    Output outputId -> World bots newOutputs
      where
        newOutputs = Map.insert outputId val outputs

botGivesValuesToThings :: Int -> Thing -> Thing -> World -> (Bool, World)
botGivesValuesToThings botId lowThing highThing world = runProgram $ do
    high <- takeFromBot botId pickHigh
    low  <- takeFromBot botId pickLow
    giveToThing lowThing low
    giveToThing highThing high
  where
    runProgram :: WorldState a -> (Bool, World)
    runProgram m = case State.execStateT m world of
        Nothing -> (False, world)
        Just newWorld -> (True, newWorld)
    takeFromBot :: Ord b => Int -> (Int -> b) -> WorldState Int
    takeFromBot botId picker = do
        bots <- State.gets theBots
        (val, rest) <- case Map.findWithDefault [] botId bots of
            [] -> mzero
            as -> return $ let l = List.sortBy (comparing picker) as in (head l, tail l)
        State.modify $ \(World bots outputs) ->
            World (Map.insert botId rest bots) outputs
        return val
    giveToThing :: Thing -> Int -> WorldState ()
    giveToThing thing val = State.modify (giveValueToThing val thing)
    pickLow = id
    pickHigh = Down

-- 3. run over commands, recording all states we pass through
--
loop :: [Cmd] -> World -> [World]
loop cmds world = case oneLoop cmds world of
    ([], worlds) -> worlds
    (cmds, worlds) -> loop cmds (head worlds) ++ worlds

oneLoop :: [Cmd] -> World -> ([Cmd], [World])
oneLoop cmds world =
    List.foldl' (\(kept,worlds) cmd -> case cmd of
        ValueGoesTo val thing ->
            (kept, giveValueToThing val thing (head worlds) : worlds)
        BotGivesValues botId low high ->
            let (success,world') = botGivesValuesToThings botId low high (head worlds)
            in if success then (kept, world':worlds) else (kept++[cmd], worlds))
        ([],[world]) cmds

main = do

    cmds <- loadCmds
    let allWorlds = (loop cmds emptyWorld)

    putStrLn "Star 1:"
    let findBot allBots = fmap fst $ List.find (\(botId,vals) -> List.any (== [61,17]) (List.permutations vals)) allBots
    print $ List.foldl' (\botId (World bots _) -> case botId of
        Nothing -> findBot (Map.toList bots)
        Just _ -> botId) Nothing allWorlds

    putStrLn "Star 2:"
    let outputs = theOutputs (head allWorlds)
    print $ (\a b c -> a * b * c) <$> Map.lookup 0 outputs <*> Map.lookup 1 outputs <*> Map.lookup 2 outputs