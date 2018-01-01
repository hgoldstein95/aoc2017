module Day20 where

import Text.KesselRun (unsafeParse)
import Text.KesselRun.Strings
import qualified Data.List as L

data Vec = Vec
  { vecX :: Integer
  , vecY :: Integer
  , vecZ :: Integer
  } deriving (Eq)

instance Show Vec where
  show (Vec x y z) = show (x, y, z)

addVec :: Vec -> Vec -> Vec
addVec (Vec x1 y1 z1) (Vec x2 y2 z2) = Vec (x1 + x2) (y1 + y2) (z1 + z2)

origin :: Vec
origin = Vec 0 0 0

data Particle = Particle
  { partName :: Integer
  , partP :: Vec
  , partV :: Vec
  , partA :: Vec
  } deriving (Show)

mkParticle :: (Integer, String) -> Particle
mkParticle (n, s) =
  unsafeParse particle s
  where particle = do
          p <- string "p=" >> vec
          v <- string ", v=" >> vec
          a <- string ", a=" >> vec
          return $ Particle n p v a
        vec = do
          i <- char '<' >> integer
          j <- char ',' >> integer
          k <- char ',' >> integer
          _ <- char '>'
          return $ Vec i j k
        integer = int >>= return . toInteger

mkParticles :: String -> [Particle]
mkParticles = map mkParticle . zip [0..] . lines

update :: Particle -> Particle
update p =
  let p' = p { partV = addVec (partV p) (partA p) }
  in p' { partP = addVec (partP p') (partV p') }

distFrom :: Particle -> Vec -> Integer
(Particle { partP = Vec x1 y1 z1 }) `distFrom` (Vec x2 y2 z2) =
  abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

closestTo :: [Particle] -> Vec -> Particle
closestTo ps v = head $ L.sortOn (`distFrom` v) ps

closestOverTime :: [Particle] -> [Integer]
closestOverTime = map partName . map (`closestTo` origin) . iterate (map update)

resolveCollision :: [Particle] -> [Particle]
resolveCollision =
  concat .
  filter (\ps -> length ps <= 1) .
  L.groupBy (\p q -> partP p == partP q)
