module Day22 where

import Prelude hiding (map,(!!),replicate,(++))
import Data.Sequence (Seq,fromList,adjust,index,(<|),(|>),replicate,(><))
import qualified Data.Sequence as S (reverse)
import Data.List (iterate)
import Data.Bifunctor (bimap,first,second)
import Data.Foldable (toList)

input = readFile "input/input22.txt"
input_test = readFile "input/input22_test.txt"

data Node = Clean | Infected | Flagged | Weakened
  deriving Eq

instance Show Node where
    show Clean = "."
    show Infected = "#"
    show Flagged = "F"
    show Weakened = "W"

type Row = (Seq Node,Seq Node)
type Grid = (Seq Row,Seq Row)

parse :: String -> Grid
parse input = let
    rawRows = lines input
    negSize = length rawRows `div` 2
    f = fromList . fmap parseNode
    rows = fmap (\row -> bimap f f (reverse $ take negSize row, drop negSize row)) rawRows
  in
    bimap fromList fromList (drop (negSize+1) rows, reverse $ take (negSize+1) rows)

(v1,v2) !! i | i < 0 = v1 `index` (abs i - 1)
(v1,v2) !! i         = v2 `index` i

parseNode '#' = Infected
parseNode '.' = Clean

type Direction = (Int,Int)
type Coordinate = (Int,Int)

data State = State {
    direction :: Direction,
    location :: Coordinate,
    infections :: Int
} deriving Show

prettyPrint :: Grid -> String
prettyPrint (bottoms,tops) = unlines $ toList $ fmap (\(bs,ts) -> toList $ foldMap show $ S.reverse bs >< ts) $ S.reverse tops >< bottoms

adj f i grid | i < 0 = first (adjust f (abs i - 1)) grid
adj f i grid         = second (adjust f i) grid

update f (x,y) = adj (adj f x) y

turnLeft (0,1)  = (-1,0)
turnLeft (-1,0) = (0,-1)
turnLeft (0,-1) = (1,0)
turnLeft (1,0)  = (0,1)

turnRight (0,1)  = (1,0)
turnRight (-1,0) = (0,1)
turnRight (0,-1) = (-1,0)
turnRight (1,0)  = (0,-1)

evolveNode False Clean = Infected
evolveNode False Infected = Clean
evolveNode evolved Clean = Weakened
evolveNode evolved Weakened = Infected
evolveNode evolved Infected = Flagged
evolveNode evolved Flagged = Clean

infectOrClean :: Bool -> Grid -> State -> Grid
infectOrClean evolved grid (State _ loc _) = update (evolveNode evolved) loc grid

updateDirection False grid state@(State dir (x,y) infs) | grid !! y !! x == Clean = state { direction = turnLeft dir, infections = infs+1 }
updateDirection False grid state@(State dir (x,y) infs)                           = state { direction = turnRight dir }

updateDirection evolved grid state@(State dir (x,y) _)    | grid !! y !! x == Infected = state { direction = turnRight dir }
updateDirection evolved grid state@(State dir (x,y) _)    | grid !! y !! x == Clean    = state { direction = turnLeft dir }
updateDirection evolved grid state@(State dir (x,y) infs) | grid !! y !! x == Weakened = state { infections = infs+1 }
updateDirection evolved grid state@(State dir (x,y) _)    | grid !! y !! x == Flagged  = state { direction = turnLeft (turnLeft dir) }

move state@(State (dx,dy) (x,y) _) = state { location = (x+dx,y+dy) }

extendRow :: Row -> Row
extendRow (a,b) = (a |> Clean, b |> Clean)

emptyCopy :: Row -> Row
emptyCopy (a,b) = (replicate (length a) Clean,replicate (length b) Clean)

extend :: Grid -> State -> Grid
extend grid (State _ (x,y) _) | (max (abs x) (abs y)) < length (fst grid) = grid
extend grid (State _ (x,y) _) = bimap (\bottoms -> fmap extendRow $ bottoms |> emptyCopy (bottoms `index` 0))
                                      (\tops -> fmap extendRow $ tops |> emptyCopy (tops `index` 0))
                                      grid

act evolved (grid,state@(State direction (x,y) _)) = let
    newState = updateDirection evolved grid state
    newGrid = infectOrClean evolved grid newState
  in
    (extend newGrid newState, move newState)

initialState = State (0,1) (0,0) 0

solv evolved iterations input = head $ drop iterations $ iterate (act evolved) (parse input, initialState)

solve evolved iterations = infections . snd . solv evolved iterations

debug evolved iterations input = solv evolved iterations <$> input >>= (putStrLn . prettyPrint . fst)

solution1 = solve False 10000 <$> input
solution2 = solve True 10000000 <$> input