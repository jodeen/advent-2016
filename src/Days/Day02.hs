module Days.Day02 (runDay) where

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
import Data.Attoparsec.Text hiding (D)
import Data.Void
import Control.Applicative ((<|>))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (many1 (parseU <|> parseD <|> parseL <|> parseR)) `sepBy1'` endOfLine
    where 
        parseU = do
            char 'U'
            return U
        parseD = do
            char 'D'
            return D
        parseL = do
            char 'L'
            return L
        parseR = do
            char 'R'
            return R                        

------------ TYPES ------------
type Input = [[Dir]]
data Dir = U | R | L | D deriving(Show)

type OutputA = String

type OutputB = String

------------ PART A ------------
doMove :: Int -> Dir -> Int
doMove 1 R = 2
doMove 1 D = 4
doMove 2 L = 1
doMove 2 R = 3
doMove 2 D = 5
doMove 3 L = 2
doMove 3 D = 6
doMove 4 U = 1
doMove 4 R = 5
doMove 4 D = 7
doMove 5 U = 2
doMove 5 L = 4
doMove 5 R = 6
doMove 5 D = 8
doMove 6 U = 3
doMove 6 L = 5
doMove 6 D = 9
doMove 7 U = 4
doMove 7 R = 8
doMove 8 U = 5
doMove 8 L = 7
doMove 8 R = 9
doMove 9 L = 8
doMove 9 U = 6
doMove d _ = d

processList :: [[Dir]] -> Int -> [Int]
processList [] _ = []
processList (x:xs) key =  newKey : processList xs newKey
    where 
        newKey = foldl doMove key x

partA :: Input -> OutputA
partA input = concatMap show (processList input 5)

------------ PART B ------------
doMove' :: Char -> Dir -> Char
doMove' '1' D = '3'
doMove' '2' R = '3'
doMove' '2' D = '6'
doMove' '3' U = '1'
doMove' '3' L = '2'
doMove' '3' R = '4'
doMove' '3' D = '7'
doMove' '4' L = '3'
doMove' '4' D = '8'
doMove' '5' R = '6'
doMove' '6' U = '2'
doMove' '6' R = '7'
doMove' '6' D = 'A'
doMove' '6' L = '5'
doMove' '7' U = '3'
doMove' '7' R = '8'
doMove' '7' D = 'B'
doMove' '7' L = '6'
doMove' '8' U = '4'
doMove' '8' R = '9'
doMove' '8' D = 'C'
doMove' '8' L = '7'
doMove' '9' L = '8'
doMove' 'A' U = '6'
doMove' 'A' R = 'B'
doMove' 'B' U = '7'
doMove' 'B' R = 'C'
doMove' 'B' D = 'D'
doMove' 'B' L = 'A'
doMove' 'C' L = 'B'
doMove' 'C' U = '8'
doMove' 'D' U = 'B'
doMove' d _ = d

processList' :: [[Dir]] -> Char -> [Char]
processList' [] _ = []
processList' (x:xs) key =  newKey : processList' xs newKey
    where 
        newKey = foldl doMove' key x

partB :: Input -> OutputB
partB input = processList' input '5'
