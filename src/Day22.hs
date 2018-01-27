module Day22 where

import Prelude hiding (replicate)
import Data.Foldable (toList)
import Data.Bifunctor (first,second)
import Data.Tuple.Extra ((&&&),both)
import qualified Data.Sequence as Seq (fromList,reverse)
import Data.Sequence (Seq,adjust,index,(|>),replicate,(><))

input      = readFile "input/input22.txt"
input_test = readFile "input/input22_test.txt"

data Node = Clean | Infected | Flagged | Weakened
  deriving Eq

instance Show Node where
    show Clean    = "."
    show Infected = "#"
    show Flagged  = "F"
    show Weakened = "W"

type Row = (Seq Node,Seq Node)
type Grid = (Seq Row,Seq Row)

parseNode '#' = Infected
parseNode '.' = Clean

-- split each line to two halves so they can grow in both directions
parseRow :: Int -> String -> Row
parseRow splitLength = both (Seq.fromList . fmap parseNode) . (reverse . take splitLength &&& drop splitLength)

-- split rows to two halves so the grid can grow in both directions
splitRows :: Int -> [Row] -> ([Row],[Row])
splitRows splitLength = (drop (splitLength+1) &&& reverse . take (splitLength+1))

parseGrid :: String -> Grid
parseGrid inp = let
    rows = lines inp
    negativeSize = length rows `div` 2
  in
    both Seq.fromList . splitRows negativeSize . fmap (parseRow negativeSize) $ rows

type Direction  = (Int,Int)
type Coordinate = (Int,Int)

data State = State {
    _direction  :: Direction,
    _location   :: Coordinate,
    _infections :: Int
} deriving Show

-- the grid is split to half, so adjust the correct index in the correct half
adjustAt f i | i < 0 = first (adjust f (abs i - 1))
adjustAt f i         = second (adjust f i)

update f (x,y) = adjustAt (adjustAt f x) y

turnLeft :: Direction -> Direction
turnLeft (0,1)  = (-1,0)
turnLeft (-1,0) = (0,-1)
turnLeft (0,-1) = (1,0)
turnLeft (1,0)  = (0,1)

turnRight = both (* (-1)) . turnLeft

data Mode = Normal | Evolved

evolveNode Normal Clean    = Infected
evolveNode Normal Infected = Clean
evolveNode _      Clean    = Weakened
evolveNode _      Weakened = Infected
evolveNode _      Infected = Flagged
evolveNode _      Flagged  = Clean

infectOrClean mode (State _ loc _) = update (evolveNode mode) loc

(v1,_) !!! i | i < 0 = v1 `index` (abs i - 1)
(_,v2) !!! i         = v2 `index` i

updateDirection Normal grid state@(State dir (x,y) infs) | grid !!! y !!! x == Clean    = state { _direction = turnLeft dir, _infections = infs+1 }
updateDirection Normal _    state@(State dir _     _)                                   = state { _direction = turnRight dir }
updateDirection _      grid state@(State dir (x,y) _)    | grid !!! y !!! x == Infected = state { _direction = turnRight dir }
updateDirection _      grid state@(State dir (x,y) _)    | grid !!! y !!! x == Clean    = state { _direction = turnLeft dir }
updateDirection _      grid state@(State _   (x,y) infs) | grid !!! y !!! x == Weakened = state { _infections = infs+1 }
updateDirection _      grid state@(State dir (x,y) _)    | grid !!! y !!! x == Flagged  = state { _direction = turnLeft (turnLeft dir) }

move state@(State (dx,dy) (x,y) _) = state { _location = (x+dx,y+dy) }

extendRow = both (|> Clean)

extendHalfGrid half = fmap extendRow $ half |> emptyCopy (half `index` 0)

emptyCopy = both (flip replicate Clean . length)

extend grid (State _ (x,y) _) | max (abs x) (abs y) < length (fst grid) = grid
extend grid (State _ _     _)                                           = both extendHalfGrid grid

burst mode (grid,state) = let
    newState = updateDirection mode grid state
    newGrid = infectOrClean mode newState grid
  in
    (extend newGrid newState, move newState)

initialState = State (0,1) (0,0) 0

solv mode iterations inp = head . drop iterations . iterate (burst mode) $ (parseGrid inp, initialState)

solve mode iterations = _infections . snd . solv mode iterations

prettyPrint (bottoms,tops) = unlines . toList . fmap (\(bs,ts) -> toList . foldMap show $ Seq.reverse bs >< ts) $ Seq.reverse tops >< bottoms

debug mode iterations inp = solv mode iterations <$> inp >>= (putStrLn . prettyPrint . fst)

-- "how many bursts cause a node to become infected"
solution1 = solve Normal 10000 <$> input

-- "how many bursts cause a node to become infected"
solution2 = solve Evolved 10000000 <$> input