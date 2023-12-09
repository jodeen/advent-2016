module Days.Day07 (runDay) where

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
import Data.Attoparsec.Text hiding (take)
import Data.Void
import Control.Applicative.Combinators (between)
import Control.Applicative ((<|>))
import Data.Either (partitionEithers)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseLine `sepBy1'` endOfLine

parseLine = many1' (Left <$> many1' letter <|> between (char '[') (char ']') (Right <$> many1' letter))

------------ TYPES ------------
type Input = [[Either String String]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
window :: Int -> String -> [String]
window num l = filter (\s -> length s == num) (map (take num) (tails l))

containsPair :: String -> Bool
containsPair item = any (\s -> s == reverse s && (head s) /= (s !! 1)) (window 4 item)

isAbba :: [Either String String] -> Bool
isAbba sections = (not (any containsPair hnet)) && (any containsPair bare)
    where
        (bare, hnet) = partitionEithers sections

partA :: Input -> OutputA
partA input = length (filter isAbba input)

------------ PART B ------------
find3 :: String -> [(Char,Char)]
find3 string = mapMaybe isAba (window 3 string)
    where
        isAba [a,b,c] = if (a == c && b /= a) then Just (a,b) else Nothing

isSsl :: [Either String String] -> Bool
isSsl sections = isJust (find (\(a,b) -> [b,a,b] `elem` babCandidates) abaCandidates)
    where
        (bare, hnet) = partitionEithers sections
        abaCandidates = concatMap find3 (concatMap (window 3) bare)
        babCandidates = concatMap (window 3) hnet

partB :: Input -> OutputB
partB input = length (filter isSsl input)