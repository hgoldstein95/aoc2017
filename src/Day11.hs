module Day11 where

import Data.List.Split (splitOn)

{-
 x
  \ n  /
nw +--+ ne
  /    \
-+      +- z
  \    /
sw +--+ se
  / s  \
 y
-}

data Hex = Hex
  { hexX :: Integer
  , hexY :: Integer
  , hexZ :: Integer
  } deriving (Show)

data Move = North
          | NorthEast
          | NorthWest
          | South
          | SouthEast
          | SouthWest
          deriving (Show)

move :: Move -> Hex -> Hex
move North h@(Hex x y _) = h { hexX = x + 1, hexY = y - 1 }
move NorthEast h@(Hex _ y z) = h { hexY = y - 1, hexZ = z + 1 }
move NorthWest h@(Hex x _ z) = h { hexX = x + 1, hexZ = z - 1 }
move South h@(Hex x y _) = h { hexX = x - 1, hexY = y + 1 }
move SouthEast h@(Hex x _ z) = h { hexX = x - 1, hexZ = z + 1 }
move SouthWest h@(Hex _ y z) = h { hexY = y + 1, hexZ = z - 1 }

toMove :: String -> Move
toMove s | s == "n" = North
         | s == "ne" = NorthEast
         | s == "nw" = NorthWest
         | s == "s" = South
         | s == "se" = SouthEast
         | s == "sw" = SouthWest
         | otherwise = error $ "parse error on input: " ++ s

toMoves :: String -> [Move]
toMoves s = map toMove $ splitOn "," s

distance :: Hex -> Hex -> Integer
distance (Hex x1 y1 z1) (Hex x2 y2 z2) =
  let dx = abs $ x2 - x1
      dy = abs $ y2 - y1
      dz = abs $ z2 - z1
  in max dx $ max dy dz

center :: Hex
center = Hex 0 0 0

moves :: [Move] -> Hex -> Hex
moves mvs st = foldl (\acc x -> move x acc) st mvs

solve :: String -> Integer
solve s = distance center $ moves (toMoves s) center

--- Part 2

hexMax :: Hex -> Hex -> Hex
hexMax h1 h2 = if (distance h1 center) > (distance h2 center) then h1 else h2

movesSave :: [Move] -> Hex -> (Hex, Hex)
movesSave mvs st = foldl accum (st, st) mvs
  where accum (acc, mx) m =
          let newH = move m acc
          in (newH, hexMax mx newH)

solve2 :: String -> Integer
solve2 s = distance center $ snd $ movesSave (toMoves s) center
