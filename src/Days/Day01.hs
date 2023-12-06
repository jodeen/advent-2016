module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Control.Applicative ((<|>))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (parseLeft <|> parseRight) `sepBy1'` (string ", ")
    where
        parseLeft = do
            char 'L'
            Left <$> decimal
        parseRight = do
            char 'R'
            Right <$> decimal    

------------ TYPES ------------
type Input = [Move]
type Move = Either Int Int
data Dir = N | E | S | W deriving (Show)
type State = (Int, Int, Dir)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
doMove :: State -> Dir -> Int -> State
doMove (x,y, d) N num = (x, y + num, N)
doMove (x,y, d) E num = (x + num, y, E)
doMove (x,y, d) S num = (x, y - num, S)
doMove (x,y, d) W num = (x - num, y, W)

doTurn :: Dir -> Move -> Dir
doTurn N (Left _) = W
doTurn N (Right _) = E
doTurn E (Left _) = N
doTurn E (Right _) = S
doTurn S (Left _) = E
doTurn S (Right _) = W
doTurn W (Left _) = S
doTurn W (Right _) = N


doStep :: State -> Move -> State
doStep (x, y, d) (Left num) = doMove (x,y,d) newDir num
    where
        newDir = doTurn d (Left num)
doStep (x, y, d) (Right num) = doMove (x,y,d) newDir num
    where
        newDir = doTurn d (Right num)


partA :: Input -> OutputA
partA input = abs x + abs y
    where
        (x, y, _) = foldl doStep (0, 0, N) input

------------ PART B ------------
doMove' :: State -> Dir -> Int -> [State]
doMove' (x,y, d) N num = [(x, y + n, N) | n <- [1..num]]
doMove' (x,y, d) E num = [(x + n, y, E) | n <- [1..num]]
doMove' (x,y, d) S num = [(x, y - n, S) | n <- [1..num]]
doMove' (x,y, d) W num = [(x - n, y, W) | n <- [1..num]]

doStep' :: [State] -> Move -> [State]
doStep' moves (Left num) = moves ++ (doMove' (x,y,d) newDir num)
    where
        newDir = doTurn d (Left num)
        (x, y, d) = last moves

doStep' moves (Right num) = moves ++ (doMove' (x,y,d) newDir num)
    where
        newDir = doTurn d (Right num)
        (x, y, d) = last moves

partB :: Input -> OutputB
partB input = abs x + abs y
    where
        path = map (\(x,y,_) -> (x,y)) (foldl doStep' [(0, 0, N)] input)
        (x, y) = last (head (filter (\p -> (last p) `elem` (init p)) (tail (inits path))   ))
    
