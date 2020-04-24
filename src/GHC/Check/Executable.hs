{- | Discover the GHC version by querying the GHC executable

 -}
module GHC.Check.Executable where

import Data.Version
import GHC.Check.Util
import System.FilePath
import System.Process
import Text.ParserCombinators.ReadP
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

-- | Takes a path to the GHC binary to query.
--   Throws if anything goes wrong.
getGhcVersion :: FilePath -> IO Version
getGhcVersion fp = do
    out <- readProcess fp ["--numeric-version"] ""
    case readP_to_S (parseVersion <* eof) (trim out) of
        [(v, "")] -> return v
        _ -> error $ "Failed to parse GHC version: " <> out

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

guessExecutablePathFromLibdir :: FilePath -> FilePath
guessExecutablePathFromLibdir fp = fp </> "bin" </> "ghc"