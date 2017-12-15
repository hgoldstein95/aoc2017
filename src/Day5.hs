module Day5 where

type Prog = ([Integer], [Integer])

fwd :: Integer -> Prog -> Maybe Prog
fwd _ (_, []) = Nothing
fwd 0 p = Just p
fwd n (fnt, (x : bck)) = fwd (n - 1) (x : fnt, bck)

rev :: Integer -> Prog -> Maybe Prog
rev _ ([], _) = Nothing
rev 0 p = Just p
rev n (x : fnt, bck) = rev (n - 1) (fnt, x : bck)

makeMove :: Prog -> Maybe Prog
makeMove (_, []) = Nothing
makeMove (fnt, x : bck) =
  let newProg = (fnt, (x + 1) : bck)
  in if x >= 0
     then fwd x newProg
     else rev (abs x) newProg

solve :: Prog -> Integer
solve = solveRec 0 . return
  where solveRec acc Nothing = acc
        solveRec acc (Just p) = solveRec (acc + 1) $ makeMove p
