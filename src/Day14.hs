module Day14 where

import Day10 (hash)
import Control.Arrow ((>>>))
import Data.Array
import Data.Char (digitToInt)
import Data.List (find)

hexToBin :: String -> String
hexToBin = concatMap convertChar
  where convertChar '0' = "0000"
        convertChar '1' = "0001"
        convertChar '2' = "0010"
        convertChar '3' = "0011"
        convertChar '4' = "0100"
        convertChar '5' = "0101"
        convertChar '6' = "0110"
        convertChar '7' = "0111"
        convertChar '8' = "1000"
        convertChar '9' = "1001"
        convertChar 'a' = "1010"
        convertChar 'b' = "1011"
        convertChar 'c' = "1100"
        convertChar 'd' = "1101"
        convertChar 'e' = "1110"
        convertChar 'f' = "1111"
        convertChar _ = error "cannot parse hex"

genGrid :: String -> [String]
genGrid s =
  map (\n -> s ++ "-" ++ show n) >>>
  map hash >>>
  map hexToBin $
  [(0 :: Int)..127]

solve :: String -> Int
solve = genGrid >>>
        map (filter (== '1')) >>>
        map length >>>
        sum

--- Part 2

type Grid = Array (Int, Int) Int

genArrayGrid :: String -> Grid
genArrayGrid s =
  let grid = map (map digitToInt) $ genGrid s
      arrList = [ ((x, y), b) |
                  (y, xs) <- zip [0..] grid,
                  (x, b) <- zip [0..] xs ]
  in array ((0, 0), (127, 127)) arrList

floodFill :: Int -> (Int, Int) -> Grid -> Grid
floodFill r n@(x, y) g =
  if not (inRange (bounds g) n) || g ! n /= 1 then g
  else floodFill r (x, y - 1) >>>
       floodFill r (x, y + 1) >>>
       floodFill r (x - 1, y) >>>
       floodFill r (x + 1, y) $
       g // [(n, r)]

solve2 :: String -> Int
solve2 s =
  sRec (genArrayGrid s) [2..]
  where sRec _ [] = 0
        sRec g (x : xs) =
          case find (\(_, i) -> i == 1) (assocs g) of
            Nothing -> x - 2
            Just (node, _) -> sRec (floodFill x node g) xs
