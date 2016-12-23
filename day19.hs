#!/usr/bin/env stack
{- stack
   --resolver lts-7.13
   --install-ghc runghc
   --package base
   --package vector
   --package containers
   --
   -hide-all-packages
-}

import qualified Data.Vector.Unboxed as Vector
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Vector.Unboxed (Vector, (!))
import Data.Monoid ((<>))
import Debug.Trace

star1 :: Int -> Int
star1 n = loop [1..n]
  where
    loop [i] = i
    loop as =
        if length as `mod` 2 == 0
            then loop (next as)
            else loop (tail $ next as)
    next (a:b:rest) = a : next rest
    next rest = rest

-- naive approach to star 2 - update vector each time we remove an item
-- very slow (though unboxed vectors help a lot)
star2 :: Int -> Int
star2 n = loop ((Vector.generate n (+1)), 0)
  where
    loop (as, idx) =
        if traceShowId (Vector.length as) == 1 then as ! 0
        else loop $ once as idx
    once as n =
        (newVec, if badN < currIdx then currIdx else currIdx+1)
      where
        newVec = h <> Vector.tail t where (h,t) = Vector.splitAt badN as
        currIdx = n `mod` l
        l = Vector.length as
        badN = (currIdx + floor (fromIntegral l / 2)) `mod` l

main = do

    putStrLn "Star 1:"
    print $ star1 3012210

    putStrLn "Star 2:"
    print $ star2 3012210