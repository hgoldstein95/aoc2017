module Day8 where

import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, letter, digit, spaces, string)
import Text.Parsec.Combinator (many1, optionMaybe)
import Control.Applicative
import Data.Either
import qualified Data.Map as M

data CondOp = Lt | Gt | Le | Ge | Eqs | Neq deriving (Show)
type Cond a = (a, CondOp, Integer)
data Op = Inc | Dec deriving (Show)

data Instr a = Instr
  { instrCond :: Cond a
  , instrOp :: Op
  , instrReg :: a
  , instrAmt :: Integer
  } deriving (Show)

parseNum :: Parser Integer
parseNum = do
  sign <- optionMaybe $ char '-'
  n <- many1 digit
  return $ case sign of
             Nothing -> read n
             Just _ -> -(read n)

parseReg :: Parser String
parseReg = many1 letter

parseLte :: Parser CondOp
parseLte = do _ <- string "<"
              m <- optionMaybe $ string "="
              return $ case m of
                         Nothing -> Lt
                         Just _ -> Le

parseGte :: Parser CondOp
parseGte = do _ <- string ">"
              m <- optionMaybe $ string "="
              return $ case m of
                         Nothing -> Gt
                         Just _ -> Ge

parseCondOp :: Parser CondOp
parseCondOp = parseLte
              <|> parseGte
              <|> (string "==" >> return Eqs)
              <|> (string "!=" >> return Neq)

parseCond :: Parser (Cond String)
parseCond = do
  r <- parseReg
  spaces
  o <- parseCondOp
  spaces
  i <- parseNum
  return (r, o, i)

parseOp :: Parser Op
parseOp = do
  s <- string "inc" <|> string "dec"
  return $ case s of
    "inc" -> Inc
    "dec" -> Dec
    _ -> error "impossible"

parseInstr :: Parser (Instr String)
parseInstr = do
  r <- parseReg
  spaces
  op <- parseOp
  spaces
  n <- parseNum
  spaces
  _ <- string "if"
  spaces
  c <- parseCond
  return $ Instr c op r n

toInstrs :: String -> [Instr String]
toInstrs s = snd $ partitionEithers $ map (parse parseInstr "") $ lines s

type Store a = M.Map a Integer

regVal :: Ord a => Store a -> a -> Integer
regVal s x = M.findWithDefault 0 x s

interpCond :: Ord a => Store a -> Cond a -> Bool
interpCond s (r, cop, v) =
  (cdn cop) (regVal s r) v
  where cdn Lt = (<)
        cdn Gt = (>)
        cdn Le = (<=)
        cdn Ge = (>=)
        cdn Eqs = (==)
        cdn Neq = (/=)

interp :: Ord a => Store a -> Instr a -> Store a
interp s (Instr c op r amt) =
  if interpCond s c then
    let rval = regVal s r
        newVal = (opfn op) rval amt
    in M.insert r newVal s
  else
    s
  where opfn Inc = (+)
        opfn Dec = (-)

runProg :: Ord a => [Instr a] -> Store a
runProg = foldl interp M.empty

largestReg :: Store a -> Integer
largestReg = foldl max 0

-- Part 2

runProgSave :: Ord a => [Instr a] -> (Integer, Store a)
runProgSave = foldl accum (0, M.empty)
  where accum (x, s) i =
          let s' = interp s i
          in (max x $ largestReg s', s')
