module Days.Day04 (runDay) where

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
import Data.Vector.Internal.Check (check)
import Data.Text (Text)
import Data.Char (chr, ord)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseLine `sepBy1'` endOfLine
    where
        parseSection = many1 letter
        parseLine = do
            name <- parseSection `sepBy1'` (char '-')
            char '-'
            sectorId <- decimal
            checksum <- between (char '[') (char ']') parseSection
            return (name, sectorId, checksum)

------------ TYPES ------------
type Input = [([String], Int, String)]

type OutputA = Int

type OutputB = Int


------------ PART A ------------
countLetters :: [String] -> [(Int, Char)]
countLetters names = map (\s -> (length s, head s)) letters
    where 
        letters = group (sort (concat names))

checksum :: [String] -> String
checksum names = take 5 (map snd (sortBy sortCount (countLetters names)))

sortCount (count1, char1) (count2, char2)
    | count1 < count2 = GT
    | count1 > count2 = LT
    | count1 == count2 = compare  char1 char2

isValid :: ([String], Int, String) -> Bool
isValid (names, _, currentChecksum) = currentChecksum == (checksum names)

partA :: Input -> OutputA
partA input = sum (map (\(_, i, _) -> i) (filter isValid input))

------------ PART B ------------

shiftChar :: Int -> Char -> Char
shiftChar shift c = chr ((mod (((ord c) - 97) + shift) 26) + 97)

doDecrypt :: Int -> String -> String
doDecrypt shift = map (shiftChar shift)

doDecryptRoom :: ([String], Int, String) -> String
doDecryptRoom (names, sector, _) = unwords (map (doDecrypt sector) names)

partB :: Input -> OutputB
partB input = sector
    where
        validRooms = filter isValid input
        Just (_, sector, _) = find (\r -> (doDecryptRoom r) == "northpole object storage") validRooms
