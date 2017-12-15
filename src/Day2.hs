module Day2 where

import Data.List
import Data.Maybe

type Row = [Integer]
type Sheet = [Row]

toSheet :: String -> Sheet
toSheet s = map (\l -> map read (words l)) $ lines s

solveRow :: Row -> Integer
solveRow lst = maximum lst - minimum lst

solve :: Sheet -> Integer
solve = foldl (\acc x -> acc + solveRow x) 0

--- Part 2

checkDivides :: (Integer, Integer) -> Maybe Integer
checkDivides (x, y) =
  let mx = maximum [x, y]
      mn = minimum [x, y]
  in if mx `mod` mn == 0 then
       Just $ mx `quot` mn
     else
       Nothing

solveRow2 :: Row -> Integer
solveRow2 xs =
  let pairs = [(x, y) | (x : ys) <- tails xs, y <- ys]
  in mapMaybe checkDivides pairs !! 0

solve2 :: Sheet -> Integer
solve2 = foldl (\acc x -> acc + solveRow2 x) 0
