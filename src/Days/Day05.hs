module Days.Day05 (runDay) where

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
import Data.Attoparsec.Text (takeText)
import Data.Digest.Pure.MD5 (md5)
import Data.Text (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy (pack)
import Data.Attoparsec.ByteString.Char8 (isSpace)
import Data.Char (isAlphaNum)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1' (satisfy isAlphaNum) 

------------ TYPES ------------
type Input = String

type OutputA = String

type OutputB = String

------------ PART A ------------
doHash :: String -> String
doHash input = show (md5 (encodeUtf8 (pack input)))

interestingHashes :: String -> [String]
interestingHashes input = filter (\h -> "00000" `isPrefixOf` h) (map (\n -> doHash (input ++ (show n))) [0..])

partA :: Input -> OutputA
partA input = map (!! 5) (take 8 (interestingHashes input))

------------ PART B ------------
allLetters :: [(Char, Char)] -> Bool
allLetters i = all (\c -> c `elem` (map fst i)) ['0'..'7']

partB :: Input -> OutputB
partB input = catMaybes chars
    where
        passwordChars = map (\s -> (s !! 5, s !! 6)) (interestingHashes input)
        Just fullPass = find allLetters (inits passwordChars)
        chars = map (\c -> lookup c fullPass) ['0'..'7']
