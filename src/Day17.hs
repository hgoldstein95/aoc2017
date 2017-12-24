{-# LANGUAGE BangPatterns #-}

module Day17 where

(%) :: Integral n => n -> n -> n
(%) = mod

data Buffer a = Buffer
  { bufferData :: [a]
  , bufferPos :: Int
  } deriving (Show)

step :: Int -> a -> Buffer a -> Buffer a
step i x (Buffer d p) =
  let newPos = (p + i) `mod` length d + 1
      (fnt, bck) = splitAt newPos d
  in Buffer { bufferData = fnt ++ (x : bck)
            , bufferPos = newPos
            }

solve :: Int -> Int
solve i =
  let (Buffer d p) = foldl (\acc x -> step i x acc) (Buffer [0] 0) [1..2017]
  in d !! (p + 1)

--- Part 2

solve2 :: Int -> Int
solve2 i = sRec 1 1 1
  where sRec 50000001 _ val = val
        sRec n pos val =
          let !newPos = (pos + i % n + 1) % n
              !newVal = if newPos == 0 then n else val
          in sRec (n + 1) newPos newVal
