module Day6 where

import Data.Vector as V
import qualified Data.Set as S

type Banks = Vector Integer

vecMax :: Ord a => a -> Vector a -> (Int, a)
vecMax y = ifoldl (\(mI, mV) i x -> if x > mV then (i, x) else (mI, mV)) (0, y)

redistribute :: Banks -> Banks
redistribute banks =
  let (maxIdx, maxVal) = vecMax 0 banks
      newBanks = update banks $ fromList [(maxIdx, 0)]
      nextIdx = (maxIdx + 1) `mod` V.length banks
  in rRec maxVal nextIdx newBanks
  where rRec 0 _ b = b
        rRec n idx b =
          let newB = update b $ fromList [(idx, b ! idx + 1)]
              newIdx = (idx + 1) `mod` V.length b
          in rRec (n - 1) newIdx newB

solve :: Banks -> Integer
solve banks = solveRec 0 banks S.empty
  where solveRec acc b s =
          if S.member b s then
            acc
          else
            solveRec (acc + 1) (redistribute b) (S.insert b s)
