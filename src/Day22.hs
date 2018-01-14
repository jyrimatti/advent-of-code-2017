module Day22 where

import Prelude hiding (replicate)
import Data.Foldable (toList)
import Data.Bifunctor (bimap,first,second)
import Data.Tuple.Extra ((&&&),both)
import qualified Data.Sequence as Seq (fromList,reverse)
import Data.Sequence (Seq,adjust,index,(|>),replicate,(><))

input      = readFile "input/input22.txt"
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

parse inp = let
    rawRows = lines inp
    negSize = length rawRows `div` 2
    f = Seq.fromList . fmap parseNode
    rows = fmap (both f . (reverse . take negSize &&& drop negSize)) rawRows
  in
    both Seq.fromList . (drop (negSize+1) &&& reverse . take (negSize+1)) $ rows

(v1,_) !!! i | i < 0 = v1 `index` (abs i - 1)
(_,v2) !!! i         = v2 `index` i

parseNode '#' = Infected
parseNode '.' = Clean

type Direction  = (Int,Int)
type Coordinate = (Int,Int)

data State = State {
    direction  :: Direction,
    location   :: Coordinate,
    infections :: Int
} deriving Show

prettyPrint (bottoms,tops) = unlines . toList . fmap (\(bs,ts) -> toList . foldMap show $ Seq.reverse bs >< ts) $ Seq.reverse tops >< bottoms

adj f i | i < 0 = first (adjust f (abs i - 1))
adj f i         = second (adjust f i)

update f (x,y) = adj (adj f x) y

turnLeft :: Direction -> Direction
turnLeft (0,1)  = (-1,0)
turnLeft (-1,0) = (0,-1)
turnLeft (0,-1) = (1,0)
turnLeft (1,0)  = (0,1)

turnRight = both (* (-1)) . turnLeft

evolveNode False Clean    = Infected
evolveNode False Infected = Clean
evolveNode _     Clean    = Weakened
evolveNode _     Weakened = Infected
evolveNode _     Infected = Flagged
evolveNode _     Flagged  = Clean

infectOrClean evolved grid (State _ loc _) = update (evolveNode evolved) loc grid

updateDirection False grid state@(State dir (x,y) infs) | grid !!! y !!! x == Clean    = state { direction = turnLeft dir, infections = infs+1 }
updateDirection False _    state@(State dir _     _)                                 = state { direction = turnRight dir }
updateDirection _     grid state@(State dir (x,y) _)    | grid !!! y !!! x == Infected = state { direction = turnRight dir }
updateDirection _     grid state@(State dir (x,y) _)    | grid !!! y !!! x == Clean    = state { direction = turnLeft dir }
updateDirection _     grid state@(State _   (x,y) infs) | grid !!! y !!! x == Weakened = state { infections = infs+1 }
updateDirection _     grid state@(State dir (x,y) _)    | grid !!! y !!! x == Flagged  = state { direction = turnLeft (turnLeft dir) }

move state@(State (dx,dy) (x,y) _) = state { location = (x+dx,y+dy) }

extendRow = both (|> Clean)

emptyCopy = both (flip replicate Clean . length)

extend grid (State _ (x,y) _) | (max (abs x) (abs y)) < length (fst grid) = grid
extend grid (State _ _     _) = bimap (\bottoms -> fmap extendRow $ bottoms |> emptyCopy (bottoms `index` 0))
                                      (\tops    -> fmap extendRow $ tops |> emptyCopy (tops `index` 0))
                                      grid

act evolved (grid,state) = let
    newState = updateDirection evolved grid state
    newGrid = infectOrClean evolved grid newState
  in
    (extend newGrid newState, move newState)

initialState = State (0,1) (0,0) 0

solv evolved iterations inp = head . drop iterations . iterate (act evolved) $ (parse inp, initialState)

solve evolved iterations = infections . snd . solv evolved iterations

debug evolved iterations inp = solv evolved iterations <$> inp >>= (putStrLn . prettyPrint . fst)

solution1 = solve False 10000 <$> input
solution2 = solve True 10000000 <$> input