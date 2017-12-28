module Day18 where

import Text.Parsec (parse, try)
import Text.Parsec.Combinator (many1, optionMaybe)
import Text.Parsec.Char (char, letter, digit, string, spaces)

import Data.Array
import Data.Either (rights)
import Data.Maybe (isNothing)
import qualified Data.Map as M

import Control.Applicative ((<|>))
import Control.Arrow ((>>>))

data Val = Reg Char | Lit Integer

instance Show Val where
  show (Reg c) = show c
  show (Lit i) = show i

data Instr = Sound Val
           | Set Val Val
           | Add Val Val
           | Mul Val Val
           | Mod Val Val
           | Recover Val
           | JumpGZ Val Val
           deriving (Show)

type Prog = Array Int Instr

toProg :: String -> Prog
toProg s =
  let lst = rights $ map (parse instr "") $ lines s
  in listArray (0, length lst - 1) lst
  where instr = try sound <|> set <|> add <|> try mul <|> rmdr <|> rcv <|> jgz
        sound = do { _ <- string "snd "; v <- val; return $ Sound v }
        set = do { _ <- string "set "; v1 <- val; v2 <- val;
                   return $ Set v1 v2 }
        add = do { _ <- string "add "; v1 <- val; v2 <- val;
                   return $ Add v1 v2; }
        mul = do { _ <- string "mul "; v1 <- val; v2 <- val;
                   return $ Mul v1 v2 }
        rmdr = do { _ <- string "mod "; v1 <- val; v2 <- val;
                    return $ Mod v1 v2 }
        rcv = do { _ <- string "rcv "; v <- val; return $ Recover v }
        jgz = do { _ <- string "jgz "; v1 <- val; v2 <- val;
                   return $ JumpGZ v1 v2 }
        val = do { res <- reg <|> lit; spaces; return res }
        reg = letter >>= return . Reg
        lit = do
          sign <- optionMaybe $ char '-'
          n <- many1 digit
          return . Lit $ case sign of
                     Nothing -> read n
                     Just _ -> -(read n)

data Env = Env
  { envPC :: Int
  , envRcv :: Maybe Int
  , envSound :: Int
  , envMap :: M.Map Char Int
  } deriving (Show)

resolve :: Env -> Val -> Int
resolve _ (Lit i) = fromIntegral i
resolve (Env _ _ _ m) (Reg r) = M.findWithDefault 0 r m

incPC :: Env -> Env
incPC e = e { envPC = (envPC e) + 1 }

execInstr :: Instr -> Env -> Env
execInstr (Sound v) e = e { envSound = resolve e v }
execInstr (Set (Reg r) v) e@(Env _ _ _ m) =
  let x = resolve e v
  in e { envMap = M.insert r x m }
execInstr (Add (Reg r) v) e@(Env _ _ _ m) =
  let x = resolve e (Reg r)
      y = resolve e v
  in e { envMap = M.insert r (x + y) m }
execInstr (Mul (Reg r) v) e@(Env _ _ _ m) =
  let x = resolve e (Reg r)
      y = resolve e v
  in e { envMap = M.insert r (x * y) m }
execInstr (Mod (Reg r) v) e@(Env _ _ _ m) =
  let x = resolve e (Reg r)
      y = resolve e v
  in e { envMap = M.insert r (x `mod` y) m }
execInstr (Recover _) e@(Env _ (Just _) _ _) = e
execInstr (Recover v) e@(Env _ Nothing s _) =
  if resolve e v /= 0 then e { envRcv = Just s } else
    e
execInstr (JumpGZ v1 v2) e@(Env pc _ _ _) =
  let x = resolve e v1
      y = resolve e v2
  in if x > 0 then e { envPC = pc + y - 1 } else e
execInstr i _ = error $ "invalid instruction: " ++ show i

exec :: Prog -> Env -> Env
exec p e@(Env pc rcv _ _) =
  if inRange (bounds p) pc && isNothing rcv then
    execInstr (p ! pc) >>>
    incPC >>>
    exec p $
    e
  else e
