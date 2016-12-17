#!/usr/bin/env stack
-- stack --resolver lts-7.13 --install-ghc runghc --package attoparsec --package containers

{-

--- Day 4: Security Through Obscurity ---

Finally, you come across an information kiosk with a list of rooms. Of course, the list is encrypted and full of decoy data, but the instructions to decode the list are barely hidden nearby. Better remove the decoy data first.

Each room consists of an encrypted name (lowercase letters separated by dashes) followed by a dash, a sector ID, and a checksum in square brackets.

A room is real (not a decoy) if the checksum is the five most common letters in the encrypted name, in order, with ties broken by alphabetization. For example:

aaaaa-bbb-z-y-x-123[abxyz] is a real room because the most common letters are a (5), b (3), and then a tie between x, y, and z, which are listed alphabetically.
a-b-c-d-e-f-g-h-987[abcde] is a real room because although the letters are all tied (1 of each), the first five are listed alphabetically.
not-a-real-room-404[oarel] is a real room.
totally-real-room-200[decoy] is not.
Of the real rooms from the list above, the sum of their sector IDs is 1514.

What is the sum of the sector IDs of the real rooms?

--- Part Two ---

With all the decoy data out of the way, it's time to decrypt this list and get moving.

The room names are encrypted by a state-of-the-art shift cipher, which is nearly unbreakable without the right software. However, the information kiosk designers at Easter Bunny HQ were not expecting to deal with a master cryptographer like yourself.

To decrypt a room name, rotate each letter forward through the alphabet a number of times equal to the room's sector ID. A becomes B, B becomes C, Z becomes A, and so on. Dashes become spaces.

For example, the real name for qzmt-zixmtkozy-ivhz-343 is very encrypted name.

What is the sector ID of the room where North Pole objects are stored?

-}

import qualified Data.Text.IO as Text
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import Data.Attoparsec.Text as Parser

-- 1. define a room data type and a parser to extract rooms from strings
--
data Room = Room
    { roomName :: String
    , roomSector :: Int
    , roomChecksum :: String }
    deriving (Show)

parseRooms :: Parser [Room]
parseRooms =
  let
    parseRoom = do
        name <- many1 letter `sepBy` char '-'
        char '-'
        sector <- decimal
        char '['
        checksum <- many1 letter
        char ']'
        return $ Room (List.concat $ List.intersperse "-" name) sector checksum
  in
    parseRoom `sepBy` endOfLine

loadRooms :: IO [Room]
loadRooms = do
    input <- Text.readFile "day04input.txt"
    return (case parseOnly parseRooms input of
        Left _ -> []
        Right rooms -> rooms)

-- 2. generate room checksum from room name, so we can check existing ones are OK
--
makeCheckSumFromName :: String -> String
makeCheckSumFromName name =
  let
    freqMap = List.foldl' (\m c -> Map.insertWith (+) c 1 m) Map.empty (filter (/= '-') name)
    byAlphaFreq = List.sortBy (\(c1,n1) (c2,n2) -> (compare n2 n1) <> (compare c1 c2)) (Map.toList freqMap)
  in
    List.take 5 $ fmap fst $ byAlphaFreq

-- 3. count up sector numbers of rooms with valid checksums
--
star1 :: [Room] -> Int
star1 rooms = List.foldl' (+) 0 $ fmap roomSector rooms

-- 3.5. decrypt the names by shifting each letter according to a sector #:
star2 :: [Room] -> [Room]
star2 rooms = filter (\r -> List.take 5 (roomName r) == "north") $ fmap decryptName rooms
  where
    decryptName room = room { roomName = fmap (shiftChar $ roomSector room) (roomName room) }
    shiftChar n c = if c == '-' then ' ' else toEnum $ (((fromEnum c - 97) + n) `mod` 26) + 97

main :: IO ()
main = do

    rooms <- loadRooms
    let goodRooms = filter (\r -> roomChecksum r == makeCheckSumFromName (roomName r)) rooms

    putStrLn "Star 1:"
    print $ star1 goodRooms

    putStrLn "Star 2:"
    print $ star2 goodRooms