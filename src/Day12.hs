module Day12 where

import Data.Graph

toAdjacency :: String -> (Int, Int, [Int])
toAdjacency s =
  let wds = words s
      (k, vs) = (head wds, tail $ tail wds)
      vs' = map (\xs -> [ c | c <- xs, not (c == ',') ]) vs
  in (read k, read k, map read vs')

toGraph :: String -> Graph
toGraph s =
  let (g, _, _) = graphFromEdges $
                  map toAdjacency $
                  lines s
  in g

solve :: String -> Int
solve s = let g = toGraph s
          in length $ reachable g 0

--- Part 2

solve2 :: String -> Int
solve2 s = let g = toGraph s
           in length $ components g
