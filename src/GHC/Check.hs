{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module GHC.Check
( VersionCheck(..)
, makeGhcVersionChecker
, checkGhcVersion
) where

import           Control.Exception
import           Data.Maybe
import           Data.Version               (Version)
import           GHC
import           GHC.Check.Executable (getGhcVersion, guessExecutablePathFromLibdir)
import           GHC.Check.PackageDb  (getPackageVersionIO)
import           GHC.Check.Util       (liftVersion, guessLibdir)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import Control.Monad (filterM)
import System.Directory (doesFileExist)
import qualified Data.List.NonEmpty as NonEmpty

data VersionCheck
    = Match
    | Mismatch { compileTime :: !Version
               , runTime     :: !Version
               }
    | Failure String
    deriving (Eq, Show)

-- | A GHC version retrieved from the ghc executable
ghcRunTimeVersion :: String -> IO Version
ghcRunTimeVersion libdir = do
    let guesses = guessExecutablePathFromLibdir libdir
    validGuesses <- filterM doesFileExist $ NonEmpty.toList guesses
    case validGuesses of
        firstGuess:_ -> getGhcVersion firstGuess
        [] -> fail $ "Unable to find the GHC executable for libdir: " <> libdir

-- | Checks if the run-time version of the @ghc@ package matches the given version.
checkGhcVersion :: String -> Version -> IO VersionCheck
checkGhcVersion libdir expectedVersion = handleErrors $ do
    v <- ghcRunTimeVersion libdir
    return $ if v == expectedVersion
                then GHC.Check.Match
                else Mismatch expectedVersion v
    where
        handleErrors = flip catches
            [Handler (throwIO @SomeAsyncException)
            ,Handler (pure . Failure . show @SomeException)
            ]

-- | @makeGhcVersionChecker libdir@ returns a computation to check the run-time
--   version of ghc against the compile-time version.
makeGhcVersionChecker :: IO (Maybe FilePath) -> TExpQ (IO VersionCheck)
makeGhcVersionChecker getLibdir = do
    libdir <- runIO $ maybe guessLibdir return =<< getLibdir
    compileTimeVersion <- runIO $ do
        mbVersion <- getPackageVersionIO libdir "ghc"
        return $ fromMaybe (error "unreachable - the ghc package is a Cabal dependency") mbVersion
    [|| checkGhcVersion libdir $$(liftVersion compileTimeVersion) ||]