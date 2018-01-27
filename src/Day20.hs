module Day20 where

import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.List (unfoldr,groupBy,sortBy)

import Text.Parsec (parse,between)
import Text.Parsec.Char (string,char)
import Text.ParserCombinators.Parsec.Number (int)

input = lines <$> readFile "input/input20.txt"

type Coordinate = (Int,Int,Int)

toList (a,b,c) = [a,b,c]

data Particle = Particle {
    _position     :: Coordinate,
    _velocity     :: Coordinate,
    _acceleration :: Coordinate
} deriving Show

particleP = Particle <$> (string "p=" *> coordinateP <* string ", ")
                     <*> (string "v=" *> coordinateP <* string ", ")
                     <*> (string "a=" *> coordinateP)

coordinateP = between (char '<') (char '>') $ (,,) <$> (int <* char ',') <*> (int <* char ',') <*> int

particle = either undefined id . parse particleP ""

update (Particle (px,py,pz) (vx,vy,vz) (ax,ay,az)) = Particle (px+vx+ax,py+vy+ay,pz+vz+az) (vx+ax,vy+ay,vz+az) (ax,ay,az)

manhattanDistance = sum . fmap abs . toList . _position

steps filt particles = unfoldr (\ps -> Just (ps, filt $ fmap update ps)) particles

zipWithIndex = zip [(0::Int)..]
smallestDistance = minimumBy (compare `on` snd)

-- ugh, assuming 10000 is enough...
-- "Which particle will stay closest to position <0,0,0>"
solve1 = (!! 5000) . fmap (fst . smallestDistance . zipWithIndex . fmap manhattanDistance) . steps id . fmap particle

removeColliding = concat . filter ((== 1) . length) . groupBy ((==) `on` _position) . sortBy (compare `on` _position)

-- ugh, assuming 10000 is enough...
-- "How many particles are left after all collisions are resolved?"
solve2 = (!! 5000) . fmap length . steps removeColliding . fmap particle

solution1 = solve1 <$> input
solution2 = solve2 <$> input