module Day9 where

unEscape :: String -> String
unEscape [] = []
unEscape ('!' : xs) = unEscape $ tail xs
unEscape (x : xs) = x : unEscape xs

data Score = Score
  { scoreTot :: Integer
  , scoreDep :: Integer
  , scoreAct :: Bool
  } deriving (Show)

processScore :: String -> Score -> Score
processScore [] s = s
processScore ('<' : xs) s = processScore xs $ s { scoreAct = False }
processScore ('>' : xs) s = processScore xs $ s { scoreAct = True }
processScore ('{' : xs) s =
  processScore xs $
  if scoreAct s then s { scoreDep = scoreDep s + 1 } else s
processScore ('}' : xs) s =
  processScore xs $
  if scoreAct s then
    s { scoreTot = scoreTot s + scoreDep s, scoreDep = scoreDep s - 1 }
  else
    s
processScore (_ : xs) s = processScore xs s

solve :: String -> Integer
solve s = scoreTot $ processScore (unEscape s) (Score 0 0 True)

--- Part 2

data Garbage = Garbage
  { garbageTot :: Integer
  , garbageAct :: Bool
  }

processGarbage :: String -> Garbage -> Garbage
processGarbage [] g = g
processGarbage ('<' : xs) g =
  processGarbage xs $
  if garbageAct g then
    g { garbageTot = garbageTot g + 1 }
  else
    g { garbageAct = True }
processGarbage ('>' : xs) g = processGarbage xs $ g { garbageAct = False }
processGarbage (_ : xs) g =
  processGarbage xs $
  if garbageAct g then g { garbageTot = garbageTot g + 1 } else g

solve2 :: String -> Integer
solve2 s = garbageTot $ processGarbage (unEscape s) (Garbage 0 False)
