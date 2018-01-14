module Day20 where

import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.List (unfoldr,groupBy,sortBy)

import Text.Parsec (parse,between)
import Text.Parsec.Char (string,char)
import Text.ParserCombinators.Parsec.Number (int)

input = lines <$> readFile "input/input20.txt"

type Coord = (Int,Int,Int)

toList (a,b,c) = [a,b,c]

data Particle = Particle {
    position :: Coord,
    velocity :: Coord,
    acceleration :: Coord
} deriving Show

particleP = Particle <$> (string "p=" *> coord <* string ", ")
                     <*> (string "v=" *> coord <* string ", ")
                     <*> (string "a=" *> coord)

coord = between (char '<') (char '>') cc

cc = (,,) <$> (int <* char ',') <*> (int <* char ',') <*> int

particle = either undefined id . parse particleP ""

update (Particle (px,py,pz) (vx,vy,vz) (ax,ay,az)) = Particle (px+vx+ax,py+vy+ay,pz+vz+az) (vx+ax,vy+ay,vz+az) (ax,ay,az)

manhattan = sum . fmap abs . toList . position

steps filt particles = unfoldr (\ps -> Just (ps, filt $ fmap update ps)) particles

withIndex = zip [(0::Int)..]
withDistance = fmap manhattan
smallestDistance = minimumBy (compare `on` snd)

-- ugh, assuming 10000 is enough...
solve1 = (!! 5000) . fmap (fst . smallestDistance . withIndex . withDistance) . steps id . fmap particle

removeColliding = concat . filter ((== 1) . length) . groupBy ((==) `on` position) . sortBy (compare `on` position)

-- ugh, assuming 10000 is enough...
solve2 = (!! 5000) . fmap length . steps removeColliding . fmap particle

solution1 = solve1 <$> input
solution2 = solve2 <$> input