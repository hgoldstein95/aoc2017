module Day19 where

import Data.Array
import Data.Maybe (fromJust)
import Data.List (elemIndex, delete)

import Debug.Trace (trace)

data Dir = Lt | Rt | Dn | Up deriving (Eq, Show)
data Entry = Vert | Horiz | Plus | Letter Char | Space deriving (Show)

opposite :: Dir -> Dir
opposite Lt = Rt
opposite Rt = Lt
opposite Dn = Up
opposite Up = Dn

type Cell = (Int, Int)
type Grid = Array Cell Entry

fromChar :: Char -> Entry
fromChar ' ' = Space
fromChar '|' = Vert
fromChar '-' = Horiz
fromChar '+' = Plus
fromChar c = Letter c

createGrid :: String -> (Grid, Cell)
createGrid s =
  let charGrid = lines s
      height = length charGrid - 1
      width = (length $ head charGrid) - 1
      idxAssoc = [ ((i, j), fromChar c)
                 | (j, xs) <- zip [0..] charGrid
                 , (i, c) <- zip [0..] xs
                 ]
      arr = array ((0, 0), (width, height)) idxAssoc
      start = (fromJust $ elemIndex '|' $ head charGrid, 0)
  in (arr, start)

data Pkt = Pkt
  { pktCell :: Cell
  , pktDir :: Dir
  , pktPath :: String
  } deriving (Show)

move :: Dir -> Pkt -> Pkt
move Up p =
  let (x, y) = pktCell p
  in p { pktCell = (x, y - 1), pktDir = Up }
move Dn p =
  let (x, y) = pktCell p
  in p { pktCell = (x, y + 1), pktDir = Dn }
move Lt p =
  let (x, y) = pktCell p
  in p { pktCell = (x - 1, y), pktDir = Lt }
move Rt p =
  let (x, y) = pktCell p
  in p { pktCell = (x + 1, y), pktDir = Rt }

possibleMoves :: Grid -> Dir -> Pkt -> [Pkt]
possibleMoves grid dir pkt =
  filter validMove $
  map (\d -> move d pkt) $
  delete (opposite dir) [Lt, Rt, Dn, Up]
  where validMove (Pkt { pktCell = c, pktDir = d }) =
          if not $ inRange (bounds grid) c then
            False
          else
            case grid ! c of
              Vert -> d == Up || d == Dn
              Horiz -> d == Lt || d == Rt
              Space -> False
              Letter _ -> True
              Plus -> True

stepGrid :: Grid -> Pkt -> Maybe Pkt
stepGrid grid pkt@(Pkt { pktCell = (x, y), pktDir = dir }) =
  case grid ! (x, y) of
    Plus -> case possibleMoves grid dir pkt of
              [] -> Nothing
              (pkt' : _) -> Just pkt'
    Letter c -> Just $ move dir $
                pkt { pktPath = (pktPath pkt) ++ [c] }
    Space -> Nothing
    _ -> Just $ move dir pkt

runGrid :: Grid -> Pkt -> Pkt
runGrid g p =
  case stepGrid g p of
    Nothing -> p
    Just p' -> runGrid g p'

solve :: String -> String
solve s =
  let (grid, start) = createGrid s
  in pktPath $ runGrid grid $ Pkt
     { pktCell = start
     , pktDir = Dn
     , pktPath = ""
     }

-- Part2

runGridCount :: Int -> Grid -> Pkt -> Int
runGridCount n g p =
  case stepGrid g p of
    Nothing -> n
    Just p' -> runGridCount (n + 1) g p'

solve2 :: String -> Int
solve2 s =
  let (grid, start) = createGrid s
  in runGridCount 0 grid $ Pkt
     { pktCell = start
     , pktDir = Dn
     , pktPath = ""
     }
