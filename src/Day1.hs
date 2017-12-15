module Day1 where

import Test.HUnit

type Captcha = [Integer]

toCaptcha :: Integer -> Captcha
toCaptcha = map (read . (:[])) . show

solve :: Captcha -> Integer
solve [] = 0
solve (x : xs) = recSolve 0 $ (x : xs) ++ [x]
  where recSolve n [] = n
        recSolve n (_ : []) = n
        recSolve n (y1 : y2 : ys) =
          recSolve (if y1 == y2 then y1 + n else n) (y2 : ys)

tests :: Test
tests =
  test
    [ "for 1122," ~: 3 ~=? (solve [1, 1, 2, 2])
    , "for 1111," ~: 4 ~=? (solve [1, 1, 1, 1])
    , "for 1234," ~: 0 ~=? (solve [1, 2, 3, 4])
    , "for 91212129," ~: 9 ~=? (solve [9, 1, 2, 1, 2, 1, 2, 9])
    ]

solve2 :: Captcha -> Integer
solve2 xs =
  let (fnt, bck) = splitAt (length xs `quot` 2) xs
      xs' = zip xs (bck ++ fnt)
  in sum $ map fst $ filter (\(x, y) -> x == y) xs'
