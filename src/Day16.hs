module Day16 where

import Text.Parsec (parse)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Char (char, letter, digit)

import Data.Array
import Data.List.Split (splitOn)
import Data.Either (rights)
import Data.Maybe (fromJust)
import qualified Data.Map as M

import Control.Applicative ((<|>))
import Control.Arrow ((>>>))

data Move a = Spin Int
            | Exchange Int Int
            | Partner a a
            deriving (Show)

type Dancers a = Array Int a

toMoves :: String -> [Move Char]
toMoves s =  rights $ map (parse move "") $ splitOn "," s
  where move = spin <|> exchange <|> partner
        spin = do
          _ <- char 's'
          n <- many1 digit
          return $ Spin $ read n
        exchange = do
          _ <- char 'x'
          i <- many1 digit
          _ <- char '/'
          j <- many1 digit
          return $ Exchange (read i) (read j)
        partner = do
          _ <- char 'p'
          x <- letter
          _ <- char '/'
          y <- letter
          return $ Partner x y

toDancers :: [a] -> Dancers a
toDancers xs = listArray (0, length xs - 1) xs

fromDancers :: Dancers a -> [a]
fromDancers = elems

dance :: Eq a => Dancers a -> Move a -> Dancers a
dance xs (Spin n) = listArray (bounds xs) (b ++ f)
  where pt = length xs - (n `mod` length xs)
        (f, b) = splitAt pt (elems xs)
dance xs (Exchange i j) =
  xs // [(i, xs ! j), (j, xs ! i)]
dance xs (Partner x y) = fmap swap xs
  where swap e | e == x = y
               | e == y = x
               | otherwise = e

routine :: Eq a => Dancers a -> [Move a] -> Dancers a
routine = foldl dance

solve :: String -> String
solve = toMoves >>>
        routine (toDancers "abcdefghijklmnop") >>>
        fromDancers

--- Part 2

findCycle :: (Ord a, Eq a) => Dancers a -> [Move a] -> (Int, Int)
findCycle ds mvs =
  let vs = map elems $ tail $ iterate (flip routine mvs) ds
  in fcRec (zip [0..] vs) M.empty
  where fcRec [] _ = (0, 0)
        fcRec ((i, pos) : xs) m =
          if M.member pos m then
            (fromJust $ M.lookup pos m, i)
          else
            fcRec xs (M.insert pos i m)

routines :: Eq a => Int -> Dancers a -> [Move a] -> Dancers a
routines n ds mvs = iterate (flip routine mvs) ds !! n

solve2 :: String -> String -> String
solve2 alph s =
  let mvs = toMoves s
      ds = toDancers alph
      (start, end) = findCycle ds mvs
      rest = (1000000000 - start) `mod` (end - start)
  in fromDancers $ routines (start + rest) ds mvs
