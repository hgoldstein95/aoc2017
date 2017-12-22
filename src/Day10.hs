module Day10 where

import Numeric (showHex)
import Data.Char (ord)
import Data.Bits (xor)
import Data.List (intercalate)
import Text.Printf (printf)

data Info a = Info
  { infoData :: [a]
  , infoPos :: Int
  , infoSkip :: Int
  } deriving (Show)

swapAt :: Int -> [a] -> [a]
swapAt l xs = let (f, b) = splitAt l xs in b ++ f

hashStep :: Int -> Info a -> Info a
hashStep l i@(Info xs pos skip) =
  let (fnt, bck) = splitAt l (swapAt pos xs)
      len = length xs
  in i { infoData = swapAt (len - pos) $ (reverse fnt) ++ bck
       , infoPos = (pos + l + skip) `mod` len
       , infoSkip = skip + 1
       }

knotHash' :: [Int] -> Int -> Int
knotHash' xs rng =
  let (Info ys _ _) = foldl (\acc x -> hashStep x acc) (Info [0..rng] 0 0) xs
  in (head ys) * (head $ tail ys)

knotHash255 :: [Int] -> Int
knotHash255 xs = knotHash' xs 255

--- Part 2

type LengthSeq = [Int]

toLengthSeq :: String -> LengthSeq
toLengthSeq s = map ord s ++ [17, 31, 73, 47, 23]

hashRound :: Info Int -> LengthSeq -> Info Int
hashRound i xs = foldl (\acc x -> hashStep x acc) i xs

groupBy :: Int -> [a] -> [[a]]
groupBy i xs = gbRec xs []
  where gbRec [] acc = acc
        gbRec ys acc =
          let (f, b) = splitAt i ys
          in gbRec b (acc ++ [f])

knotHash :: LengthSeq -> String
knotHash xs =
  let sparse = infoData $
        foldl (\acc _ -> hashRound acc xs) (Info [0..255] 0 0) [(0 :: Int)..63]
  in intercalate "" $
     map (printf "%0x") $
     map (foldl xor 0) $
     groupBy 16 sparse
