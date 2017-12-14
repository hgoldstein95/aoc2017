module Day2 where

import Test.HUnit

type Row = [Integer]
type Sheet = [Row]

toSheet :: String -> Sheet
toSheet s = map (\l -> map read (words l)) $ lines s

solveRow :: Row -> Integer
solveRow lst = maximum lst - minimum lst

solve :: Sheet -> Integer
solve = foldl (\acc x -> acc + solveRow x) 0

tests :: Test
tests =
  test ["main case," ~: 18 ~=? solve [[5, 1, 9, 5], [7, 5, 3], [2, 4, 6, 8]]]
