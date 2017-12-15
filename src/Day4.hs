module Day4 where

import Data.List

hasDups :: Ord a => [a] -> Bool
hasDups = hdRec . sort
  where hdRec [] = False
        hdRec (x : xs) = if elem x xs then True else hdRec xs

solve :: [[String]] -> Integer
solve = sum . (map isValid)
  where isValid ls = if hasDups ls then 0 else 1
