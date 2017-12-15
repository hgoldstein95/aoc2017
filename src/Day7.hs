module Day7 where

import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, letter, digit, spaces, string)
import Text.Parsec.Combinator (many1, sepBy)
import Control.Applicative
import Data.Either
import qualified Data.Set as S

data ProgDecl = NoDisc String Integer
              | Disc String Integer [String]
              deriving (Show)

parseRest :: String -> Integer -> Parser ProgDecl
parseRest s d = do
  spaces
  _ <- string "->"
  spaces
  ns <- (many1 letter) `sepBy` (string ", ")
  return $ Disc s d ns

parseDecl :: Parser ProgDecl
parseDecl = do
  s <- many1 letter
  spaces
  _ <- char '('
  d <- many1 digit
  _ <- char ')'
  (parseRest s $ read d) <|> (return $ NoDisc s (read d))

toDecls :: String -> [ProgDecl]
toDecls s = snd $ partitionEithers $ map (parse parseDecl "") $ lines s

solve :: [ProgDecl] -> String
solve xs =
  let s = foldl addDecl S.empty xs
  in (filter (\x -> S.notMember x s) $ map getName xs) !! 0
  where addDecl s (NoDisc _ _) = s
        addDecl s (Disc _ _ ns) = S.union s (S.fromList ns)

        getName (NoDisc s _) = s
        getName (Disc s _ _) = s

--- Part 2
