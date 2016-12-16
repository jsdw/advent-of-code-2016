#!/usr/bin/env stack
-- stack --resolver lts-7.13 --install-ghc runghc --package attoparsec --package containers

import qualified Data.Text.IO as Text
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Attoparsec.Text as Parser
import Control.Applicative ((<|>))

-- 1. Parse our "IP addresses" into something easy to work with
--
type IP = [IPPiece]
data IPPiece = Inner String | Outer String deriving Show

parseIPs :: Parser [IP]
parseIPs = parseIp `sepBy` endOfLine
  where
    parseInner = do
        char '['
        ls <- many1 letter
        char ']'
        return (Inner ls)
    parseOuter = do
        ls <- many1 letter
        return (Outer ls)
    parseIp = do
        many1 (parseInner <|> parseOuter)

loadInput :: IO [IP]
loadInput = do
    input <- Text.readFile "day7input.txt"
    return $ case parseOnly parseIPs input of
        Left _ -> []
        Right ips -> ips

-- 2. check for ABBA lark
--

groupPieces :: IP -> ([String],[String])
groupPieces ip = List.foldl' (\(i,o) bit -> case bit of
    Outer str -> (i,str:o)
    Inner str -> (str:i,o)) ([],[]) ip

isABBAin :: String -> Bool
isABBAin str = case str of
    a:b:c:d:rest -> if a == d && b == c && a /= b then True else isABBAin (b:c:d:rest)
    _ -> False

supportsTLS :: IP -> Bool
supportsTLS ip = any isABBAin outer && all (not . isABBAin) inner
  where
    (inner,outer) = groupPieces ip

-- 3. check for ABA and corresponding BAB for star 2
--

findABAs :: String -> [(Char,Char)]
findABAs str = case str of
    a:b:c:rest -> if a == c && a /= b then (a,b) : findABAs (b:c:rest) else findABAs (b:c:rest)
    _ -> []

supportsSSL :: IP -> Bool
supportsSSL ip = not $ Set.null $ Set.intersection (toBABs allInners) allOuters
  where
      toBABs set = Set.map (\(a,b) -> (b,a)) set
      allInners = Set.fromList $ concat $ fmap findABAs inner
      allOuters = Set.fromList $ concat $ fmap findABAs outer
      (inner,outer) = groupPieces ip

main = do

    ips <- loadInput

    putStrLn "Star 1:"
    print $ length $ filter supportsTLS ips

    putStrLn "Star 2:"
    print $ length $ filter supportsSSL ips