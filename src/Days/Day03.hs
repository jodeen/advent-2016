module Days.Day03 (runDay) where

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
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseLine `sepBy1'` endOfLine
    where
        parseLine = do
            skipSpace
            a <- decimal
            skipSpace
            b <- decimal
            skipSpace
            c <- decimal
            return (a,b,c)

------------ TYPES ------------
type Input = [(Int, Int, Int)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
isTriangle :: (Int, Int, Int) -> Bool
isTriangle (a,b,c) = (a + b > c) && (a + c > b) && (b + c > a)
partA :: Input -> OutputA
partA input = length (filter isTriangle input)

------------ PART B ------------
listToTriangle :: [Int] -> [(Int, Int, Int)]
listToTriangle [] = []
listToTriangle (a:b:c:xs) = (a,b,c) : listToTriangle xs

partB :: Input -> OutputB
partB input = length (filter isTriangle newTriangles)
    where
        (a, b, c) = unzip3 input
        newTriangles = listToTriangle (a ++ b ++ c)
