#!/usr/bin/env stack
{- stack
   --resolver lts-7.13
   --install-ghc runghc
   --package base
   --
   -hide-all-packages
-}

import qualified Data.List as List 
import Debug.Trace

data Bit = O | I deriving (Show, Eq)
type Bits = [Bit]

input = toInput "01000100010010111"

toInput :: String -> Bits
toInput = fmap (\c -> if c == '0' then O else I)

showBits :: Bits -> String
showBits = fmap (\bit -> if bit == O then '0' else '1')

flipBit :: Bit -> Bit
flipBit O = I
flipBit I = O

randomData :: Bits -> Bits
randomData bits = bits ++ next bits
  where
    next bits = tail bits ++ next (bits ++ tail bits) 
    tail bits = O : fmap flipBit (List.reverse bits)

generateChecksum :: Bits -> Bits
generateChecksum bits = loop bits
  where
    loop bits =
      let bits' = step bits
      in if List.length bits' `mod` 2 /= 0 then bits' else loop bits'
    step bits = case bits of
      (a:b:rest) -> (if a == b then I else O) : step rest
      _ -> []
  
main :: IO ()
main = do 
  
    putStrLn "Star 1:"
    print $ showBits $ generateChecksum $ take 272 $ randomData input

    putStrLn "Star 2:"
    print $ showBits $ generateChecksum $ take 35651584 $ randomData input
