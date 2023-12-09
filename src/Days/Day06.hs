module Days.Day06 (runDay) where

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
inputParser = (many1' letter) `sepBy1'` endOfLine

------------ TYPES ------------
type Input = [String]

type OutputA = String

type OutputB = String

------------ PART A ------------
mostFrequent :: String -> Char
mostFrequent s =  snd (maximum (map (\c -> (length c, head c)) (group (sort s))))

partA :: Input -> OutputA
partA input = map mostFrequent frequency
    where
        frequency = transpose input

------------ PART B ------------
leastFrequent :: String -> Char
leastFrequent s =  snd (minimum (map (\c -> (length c, head c)) (group (sort s))))

partB :: Input -> OutputB
partB input = map leastFrequent frequency
    where
        frequency = transpose input
