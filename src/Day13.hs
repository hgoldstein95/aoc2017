module Day13 where

import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.List (elemIndex)

data Firewall = Firewall
  { depth :: Int
  , range :: Int
  , scanner :: (Int, Bool)
  } deriving (Show)

toFirewalls :: String -> [Firewall]
toFirewalls s =
  map (\xs -> Firewall (read $ head xs) (read $ head $ tail xs) (0, True)) $
  map (splitOn ": ") $
  lines s

stepScanner :: Firewall -> Firewall
stepScanner f@(Firewall _ 0 _) = f
stepScanner f@(Firewall _ r (s, dn)) =
  if dn then
    if s + 1 < r then
      f { scanner = (s + 1, dn) }
    else
      f { scanner = (s - 1, not dn) }
  else
    if s - 1 >= 0 then
      f { scanner = (s - 1, dn) }
    else
      f { scanner = (s + 1, not dn) }

caught :: Firewall -> Bool
caught (Firewall _ _ (s, _)) = s == 0

severity :: Firewall -> Int
severity (Firewall d r _) = d * r

tripSeverity :: [Firewall] -> Int -> Int
tripSeverity fws delay = foldl accum 0 fws
  where accum acc f@(Firewall d _ _) =
          if caught (iterate stepScanner f !! (d + delay)) then
            acc + severity f
          else
            acc

solve :: String -> Int
solve s = tripSeverity (toFirewalls s) 0

--- Part 2

solve2 :: String -> Int
solve2 s =
  let fws = toFirewalls s
      getsCaught delay = and $
        map (\(Firewall d r _) -> (d + delay) `mod` (2 * r - 2) /= 0) fws
      caughtList = map getsCaught [0..]
  in fromJust $ elemIndex True caughtList
