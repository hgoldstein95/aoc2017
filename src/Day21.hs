module Day21 where

import Data.Matrix
import qualified Data.List as L
import qualified Data.List.Split as L
import qualified Data.Map as M
import Data.Maybe (fromJust)

newtype Picture = Picture { fromPicture :: Matrix Bool } deriving (Eq)

instance Show Picture where
  show = show . fromPicture

primes :: [Integer]
primes = sieve (2 : [3, 5..])
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]
    sieve _ = error "impossible"

instance Ord Picture where
  a <= b = uniqueNum a <= uniqueNum b
    where uniqueNum = foldl (*) 1 .
                      filter (/= 0) .
                      map (\(x, y) -> x * y) .
                      zip primes .
                      map bToI .
                      toList .
                      fromPicture
          bToI True = 1
          bToI False = 0

initial :: Picture
initial = Picture $ fromLists $
          [ [False, True, False]
          , [False, False, True]
          , [True, True, True]
          ]

printPicture :: Picture -> String
printPicture = prettyMatrix . fmap printElem . fromPicture
  where printElem True = '#'
        printElem False = '.'

rotCw :: Picture -> Picture
rotCw = Picture .
        forceMatrix .
        fromLists .
        map reverse .
        toLists .
        transpose .
        fromPicture

rotations :: Picture -> [Picture]
rotations = take 4 . iterate rotCw

parseProd :: String -> (Picture, Picture)
parseProd s =
  let wds = words s
      from = wds !! 0
      to = wds !! 2
  in (parsePic from, parsePic to)
  where parsePic = Picture . fromLists . map (map (== '#')) . L.splitOn "/"

prodMap :: [(Picture, Picture)] -> M.Map Picture Picture
prodMap xs = M.fromList $ do
  (from, to) <- xs
  map (\f -> (f, to)) $ rotations from

size :: Picture -> Int
size = nrows . fromPicture

zipList :: [[a]] -> [[a]]
zipList xs =
  if L.null $ head xs then [] else (map head xs) : (zipList $ map tail xs)

splitPicture :: Picture -> Matrix Picture
splitPicture pic =
  let n = if size pic `mod` 2 == 0 then 2 else 3
  in fromLists .
     map (map (Picture . fromLists)) .
     map zipList .
     L.chunksOf n .
     map (L.chunksOf n) .
     toLists $ fromPicture pic

enhance :: M.Map Picture Picture -> Picture -> Picture
enhance m pic =
  if M.member pic m then
    fromJust $ M.lookup pic m
  else
    error $ "cannot find " ++ show pic

step :: M.Map Picture Picture -> Picture -> Picture
step m = Picture . flatten . fmap (fromPicture . enhance m) . splitPicture

solve :: String -> Int -> Int
solve s n =
  let m = prodMap . map parseProd $ lines s
  in countOn $ iterate (step m) initial !! n
  where countOn = length . filter id . toList . fromPicture
