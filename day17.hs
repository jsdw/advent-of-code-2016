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

{-# LANGUAGE BangPatterns #-}

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

-- 2. find the shortest sequence of moves that works out
--

solve :: State -> (Int,Int) -> State
solve initialS finalPos = loop [initialS]
  where
    loop ss =
      let nextStates = List.concat $ fmap moves ss
      in case List.find ((==finalPos) . statePos) nextStates of
          Nothing -> loop nextStates
          Just finalState -> finalState

main :: IO ()
main = do

    putStrLn "Star 1:"
    print $ solve (State (0,0) Seq.empty) (3,3)