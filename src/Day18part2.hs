{-# LANGUAGE BangPatterns #-}

module Day18part2 where

import qualified Day18
import Day18 (Val(Lit, Reg))

import Data.Array
import qualified Data.Map as M

import Debug.Trace (trace)

data Instr = Send Val
           | Set Val Val
           | Add Val Val
           | Mul Val Val
           | Mod Val Val
           | Recieve Val
           | JumpGZ Val Val
           deriving (Show)

type Prog = Array Integer Instr

toProg :: String -> Prog
toProg s =
  let lst = map fixInstr $ elems $ Day18.toProg s
  in listArray (0, fromIntegral $ length lst - 1) lst
  where fixInstr (Day18.Sound x) = Send x
        fixInstr (Day18.Set x y) = Set x y
        fixInstr (Day18.Add x y) = Add x y
        fixInstr (Day18.Mul x y) = Mul x y
        fixInstr (Day18.Mod x y) = Mod x y
        fixInstr (Day18.Recover x) = Recieve x
        fixInstr (Day18.JumpGZ x y) = JumpGZ x y

data Env = Env
  { envPC :: Integer
  , envMap :: M.Map Char Integer
  , envQueue :: [Integer]
  , envWait :: Integer
  , envPid :: Integer
  , envSendCount :: Integer
  } deriving (Show)

resolve :: Env -> Val -> Integer
resolve _ (Lit i) = i
resolve e (Reg r) =
  if r == 'p' then envPid e else M.findWithDefault 0 r (envMap e)

incPC :: Env -> Env
incPC e = e { envPC = envPC e + 1 }

withNothing :: a -> (a, Maybe b)
withNothing x = (x, Nothing)

insert :: Char -> Integer -> Env -> Env
insert r v e = e { envMap = M.insert r v (envMap e) }

update :: Char -> (Integer -> Integer) -> Env -> Env
update r f e =
  let currV = resolve e (Reg r)
  in e { envMap = M.insert r (f currV) (envMap e) }

execInstr :: Instr -> Env -> (Env, Maybe Integer)
execInstr (Send x) e =
  let ct = envSendCount e
  in (incPC $ e { envSendCount = ct + 1 }, Just $ resolve e x)
execInstr (Set (Reg r) y) e =
  withNothing $
  let yVal = resolve e y
  in incPC $ insert r yVal e
execInstr (Add (Reg r) y) e =
  withNothing $
  let yVal = resolve e y
  in incPC $ update r (+ yVal) e
execInstr (Mul (Reg r) y) e =
  withNothing $
  let yVal = resolve e y
  in incPC $ update r (* yVal) e
execInstr (Mod (Reg r) y) e =
  withNothing $
  let yVal = resolve e y
  in incPC $ update r (flip mod yVal) e
execInstr (Recieve (Reg r)) e =
  withNothing $
  case envQueue e of
    [] -> e { envWait = envWait e + 1 }
    (x : xs) -> incPC $ insert r x $ e { envWait = 0, envQueue = xs }
execInstr (JumpGZ x y) e =
  withNothing $
  let xVal = resolve e x
      yVal = resolve e y
      pc = envPC e
  in if xVal > 0 then e { envPC = pc + yVal } else incPC e
execInstr i _ = error $ "invalid instruction: " ++ show i

addQueue :: Integer -> Env -> Env
addQueue i e = e { envQueue = envQueue e ++ [i] }

halt :: Prog -> Env -> Bool
halt p e = not $ inRange (bounds p) (envPC e)

step :: Prog -> Env -> (Env, Env -> Env)
step prog e =
  let pc = envPC e
  in if not $ halt prog e then
       case execInstr (prog ! pc) e of
         (e', Nothing) -> (e', id)
         (e', Just i) -> (e', addQueue i)
     else
       (e, id)

exec :: Bool -> Prog -> (Env, Env) -> (Env, Env)
exec sel prog (a, b) =
  let wA = envWait a > 1
      wB = envWait b > 1
      hA = halt prog a
      hB = halt prog b
  in if (wA && wB) || (wA && hB) || (hA && wB) || (hA && hB) then
       (a, b)
     else
       exec (not sel) prog $
       if sel then
         let (a', stepB) = step prog a
         in (a', stepB b)
       else
         let (b', stepA) = step prog b
         in (stepA a, b')

freshEnv :: Integer -> Env
freshEnv i = Env
  { envPC = 0
  , envMap = M.empty
  , envQueue = []
  , envWait = 0
  , envPid = i
  , envSendCount = 0
  }

solve :: String -> Integer
solve s =
  let !prog = toProg s
      a = freshEnv 0
      b = freshEnv 1
  in envSendCount $ snd $ exec True prog (a, b)

-- IMPORTANT!
-- THIS IS NOT CORRECT. Not sure what happened.
