module Day20 where

import Text.Parsec (parse,many1,(<|>),between)
import Text.Parsec.Char (string,digit,char)
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.List (unfoldr,groupBy,sortBy)

input = lines <$> readFile "input/input20.txt"

type Coord = (Int,Int,Int)

data Particle = Particle {
    position :: Coord,
    velocity :: Coord,
    acceleration :: Coord
} deriving Show

numberP = read <$> many1 (digit <|> char '-')

particleP = Particle <$> (string "p=" *> coord <* string ", ")
                     <*> (string "v=" *> coord <* string ", ")
                     <*> (string "a=" *> coord)

coord = between (char '<') (char '>') cc

cc = (\a b c -> (a,b,c)) <$> (numberP <* char ',') <*> (numberP <* char ',') <*> numberP

particle = either undefined id . parse particleP ""

update (Particle (px,py,pz) (vx,vy,vz) (ax,ay,az)) = Particle (px+vx+ax,py+vy+ay,pz+vz+az) (vx+ax,vy+ay,vz+az) (ax,ay,az)

manhattan (Particle (px,py,pz) _ _) = abs px + abs py + abs pz

steps filt particles = unfoldr (\ps -> Just (ps, filt $ fmap update ps)) particles

withIndex = zip [(0::Int)..]
withDistance = fmap (\(i,p) -> (i,manhattan p))
smallestDistance = minimumBy (compare `on` snd)

-- ugh, assuming 10000 is enough...
solve1 = head . drop 5000 . fmap fst . fmap smallestDistance . fmap withDistance . fmap withIndex . steps (filter $ const True) . fmap particle

removeColliding = concat . filter ((== 1) . length) . groupBy (\p1 p2 -> position p1 == position p2) . sortBy (compare `on` position)

-- ugh, assuming 10000 is enough...
solve2 = head . drop 5000 . fmap length . steps removeColliding . fmap particle

solution1 = solve1 <$> input
solution2 = solve2 <$> input