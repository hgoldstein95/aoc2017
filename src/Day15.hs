module Day15 where

import Data.Maybe (catMaybes)
import Data.Bits ((.&.))

data Gen = Gen
  { genFactor :: Int
  , genSeed :: Int
  }

nextVal ::  Gen -> Maybe Int -> Int
nextVal (Gen f s) prev =
  let p = case prev of
            Just n -> n
            Nothing -> s
  in (p * f) `mod` 2147483647

genA :: Int -> Gen
genA = Gen 16807

genB :: Int -> Gen
genB = Gen 48271

genStream :: Gen -> [Int]
genStream g = catMaybes $ iterate (Just . nextVal g) Nothing

checkMatch :: Int -> Int -> Bool
checkMatch v1 v2 = (v1 .&. 65535) == (v2 .&. 65535)

solve :: (Int, Int) -> Int
solve (aSeed, bSeed) =
  let aStream = genStream $ genA aSeed
      bStream = genStream $ genB bSeed
  in foldl (\acc (vA, vB) -> if checkMatch vA vB then acc + 1 else acc) 0 $
     take 40000000 $
     zip aStream bStream

--- Part 2

solve2 :: (Int, Int) -> Int
solve2 (aSeed, bSeed) =
  let aStream = genStream $ genA aSeed
      bStream = genStream $ genB bSeed
  in foldl (\acc (vA, vB) -> if checkMatch vA vB then acc + 1 else acc) 0 $
     take 5000000 $
     zip
     (filter (\x -> x `mod` 4 == 0) aStream)
     (filter (\x -> x `mod` 8 == 0) bStream)
