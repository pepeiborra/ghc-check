{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module GHC.Check
( VersionCheck(..)
, makeGhcVersionChecker
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

data VersionCheck
    = Match
    | Mismatch { compileTime :: !Version
               , runTime     :: !Version
               }
    | Failure String
    deriving (Eq, Show)

-- | A GHC version retrieved from the ghc executable
ghcRunTimeVersion :: IO Version
ghcRunTimeVersion = do
    libdir <- guessLibdir
    getGhcVersion (guessExecutablePathFromLibdir libdir)

-- | Checks if the run-time version of the @ghc@ package matches the given version.
checkGhcVersion :: Version -> IO VersionCheck
checkGhcVersion expectedVersion = handleErrors $ do
    v <- ghcRunTimeVersion
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
    compileTimeVersion <- runIO $ do
        libdir <- maybe guessLibdir return =<< getLibdir
        mbVersion <- getPackageVersionIO libdir "ghc"
        return $ fromMaybe (error "unreachable - the ghc package is a Cabal dependency") mbVersion
    [|| checkGhcVersion $$(liftVersion compileTimeVersion) ||]