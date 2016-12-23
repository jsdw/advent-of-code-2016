#!/usr/bin/env stack
{- stack
   --resolver lts-7.13
   --install-ghc runghc
   --package base
   --package bytestring
   --package pureMD5
   --package containers
   --
   -hide-all-packages
-}

{-

--- Day 17: Two Steps Forward ---

You're trying to access a secure vault protected by a 4x4 grid of small rooms connected by doors. You start in the top-left room (marked S), and you can access the vault (marked V) once you reach the bottom-right room:

#########
#S| | | #
#-#-#-#-#
# | | | #
#-#-#-#-#
# | | | #
#-#-#-#-#
# | | |
####### V

Fixed walls are marked with #, and doors are marked with - or |.

The doors in your current room are either open or closed (and locked) based on the hexadecimal MD5 hash of a passcode (your puzzle input) followed by a sequence of uppercase characters representing the path you have taken so far (U for up, D for down, L for left, and R for right).

Only the first four characters of the hash are used; they represent, respectively, the doors up, down, left, and right from your current position. Any b, c, d, e, or f means that the corresponding door is open; any other character (any number or a) means that the corresponding door is closed and locked.

To access the vault, all you need to do is reach the bottom-right room; reaching this room opens the vault and all doors in the maze.

For example, suppose the passcode is hijkl. Initially, you have taken no steps, and so your path is empty: you simply find the MD5 hash of hijkl alone. The first four characters of this hash are ced9, which indicate that up is open (c), down is open (e), left is open (d), and right is closed and locked (9). Because you start in the top-left corner, there are no "up" or "left" doors to be open, so your only choice is down.

Next, having gone only one step (down, or D), you find the hash of hijklD. This produces f2bc, which indicates that you can go back up, left (but that's a wall), or right. Going right means hashing hijklDR to get 5745 - all doors closed and locked. However, going up instead is worthwhile: even though it returns you to the room you started in, your path would then be DU, opening a different set of doors.

After going DU (and then hashing hijklDU to get 528e), only the right door is open; after going DUR, all doors lock. (Fortunately, your actual passcode is not hijkl).

Passcodes actually used by Easter Bunny Vault Security do allow access to the vault if you know the right path. For example:

If your passcode were ihgpwlah, the shortest path would be DDRRRD.
With kglvqrro, the shortest path would be DDUDRLRRUDRD.
With ulqzkmiv, the shortest would be DRURDRUDDLLDLUURRDULRLDUUDDDRR.
Given your vault's passcode, what is the shortest path (the actual path, not just the length) to reach the vault?

--- Part Two ---

You're curious how robust this security solution really is, and so you decide to find longer and longer paths which still provide access to the vault. You remember that paths always end the first time they reach the bottom-right room (that is, they can never pass through it, only end in it).

For example:

If your passcode were ihgpwlah, the longest path would take 370 steps.
With kglvqrro, the longest path would be 492 steps long.
With ulqzkmiv, the longest path would be 830 steps long.
What is the length of the longest path that reaches the vault?

-}

import qualified Data.Sequence as Seq
import qualified Data.ByteString.Lazy.Char8 as LazyByteChars
import qualified Data.ByteString as Bytes
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.Foldable (toList)
import Data.Bits (shiftR, (.&.))
import Data.Sequence (Seq, (|>))
import Data.Digest.Pure.MD5 (md5, md5DigestBytes)

-- 1. the rules of the game
--

data Move = U | D | L | R deriving (Show, Ord, Eq)
data State = State { statePos :: (Int,Int), stateMoves :: Seq Move } deriving (Show, Ord, Eq)

input = "pslxynzg"

md5hex str = md5DigestBytes $ md5 $ LazyByteChars.pack str

showMove :: Move -> Char
showMove m = case m of
    U -> 'U'
    D -> 'D'
    L -> 'L'
    R -> 'R'

showMoves :: Seq Move -> String
showMoves ms = fmap showMove $ toList ms

moves :: State -> [State]
moves (State (x,y) moves) = Maybe.catMaybes maybeMoves
  where
    maybeMoves =
        [ if canUp    && y-1 >= 0 then Just $ State (x, y-1) (moves |> U) else Nothing
        , if canDown  && y+1 < 4  then Just $ State (x, y+1) (moves |> D) else Nothing
        , if canLeft  && x-1 >= 0 then Just $ State (x-1, y) (moves |> L) else Nothing
        , if canRight && x+1 < 4  then Just $ State (x+1, y) (moves |> R) else Nothing ]
    (canUp,canDown,canLeft,canRight) =
        (isOk (b1 `shiftR` 4), isOk (b1 .&. 15), isOk (b2 `shiftR` 4), isOk (b2 .&. 15))
      where
        [b1,b2] = Bytes.unpack $ Bytes.take 2 $ md5hex $ input ++ showMoves moves
        isOk w = w > 10 -- i.e w has hex of b,c,d,e,f

-- 2. find all of the solutions given a first state and final pos
--

solve :: State -> (Int,Int) -> [State]
solve initialS finalPos = loop [initialS]
  where
    loop ss =
      let (finished,nextStates) = List.partition ((== finalPos) . statePos) $ List.concat $ fmap moves ss
      in finished ++ (if List.null nextStates then [] else loop nextStates)

main :: IO ()
main = do

    let solutions = solve (State (0,0) Seq.empty) (3,3)

    putStrLn "Star 1:"
    putStrLn $ showMoves $ stateMoves $ head $ solutions

    putStrLn "Star 2:"
    print $ length $ showMoves $ stateMoves $ last $ solutions