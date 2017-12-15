module Day3 where

type Coord = (Integer, Integer)
data Move = Up | Dn | Lt | Rt deriving (Show)

moves :: [Move]
moves =
  let nums = concatMap (replicate 2) [1..]
      dirCycle = (concat $ repeat [Rt, Up, Lt, Dn])
  in concatMap (\(n, d) -> replicate n d) $ zip nums dirCycle

coords :: Integer -> Coord
coords n = cRec (take (fromIntegral (n - 1)) moves)
  where cRec [] = (0, 0)
        cRec (m : ms) =
          let (x, y) = cRec ms
          in case m of
            Up -> (x, y + 1)
            Dn -> (x, y - 1)
            Lt -> (x - 1, y)
            Rt -> (x + 1, y)

manhattan :: Coord -> Coord -> Integer
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

solve :: Integer -> Integer
solve n = manhattan (0, 0) $ coords n

-- Part 2: https://oeis.org/A141481
