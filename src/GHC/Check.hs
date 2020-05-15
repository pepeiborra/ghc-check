{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module GHC.Check
  ( GhcVersionChecker,
    InstallationCheck(..),
    PackageCheck,
    PackageMismatch (..),
    makeGhcVersionChecker,
    checkGhcVersion,
  )
where

import Control.Exception
import Control.Monad (filterM)
import Data.Function (on)
import Data.List (intersectBy)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid (First (First), getFirst)
import Data.Version (Version)
import GHC (Ghc, getSessionDynFlags, runGhc, setSessionDynFlags)
import GHC.Check.Executable (getGhcVersion, guessExecutablePathFromLibdir)
import GHC.Check.PackageDb (PackageVersion (..), getPackageVersion, version)
import GHC.Check.Util (liftTyped)
import Language.Haskell.TH (TExpQ, runIO)
import Language.Haskell.TH.Syntax (Lift (lift))
import System.Directory (doesFileExist)

-- | Given a run-time libdir, checks the ghc installation and returns
--   a 'Ghc' action to check the package database
type GhcVersionChecker = String -> IO InstallationCheck

data InstallationCheck
  = InstallationChecked (Ghc PackageCheck)
  | InstallationMismatch { compileTime, runTime :: !Version}

type PackageCheck = Maybe (String, PackageMismatch)

data PackageMismatch
  = VersionMismatch { compileTime, runTime :: !Version }
  | AbiMismatch { compileTimeAbi, runTimeAbi :: !String }
  deriving (Eq, Show)

comparePackageVersions :: PackageVersion -> PackageVersion -> Maybe PackageMismatch
comparePackageVersions compile run
  | compile == run = Nothing
  | version compile == version run =
    Just $ AbiMismatch (abi compile) (abi run)
  | otherwise =
    Just $ VersionMismatch (version compile) (version run)

collectPackageVersions :: [String] -> Ghc [(String, PackageVersion)]
collectPackageVersions =
  fmap catMaybes . mapM (\p -> fmap (p,) <$> getPackageVersion p)

-- | Checks if the run-time version of the @ghc@ package matches the given version.
checkGhcVersion ::
  [String] ->
  [(String, PackageVersion)] ->
  GhcVersionChecker
checkGhcVersion trackedPackages compileTimeVersions runTimeLibdir = do
  let compileTimeVersionsMap = Map.fromList compileTimeVersions
      compileTime = version $ compileTimeVersionsMap Map.! "ghc"

  runTime <- ghcRunTimeVersion runTimeLibdir

  return $ if runTime /= compileTime
    then InstallationMismatch{..}
    else InstallationChecked $ do
      runTimeVersions <- collectPackageVersions trackedPackages
      let compares =
            Map.intersectionWith
              comparePackageVersions
              compileTimeVersionsMap
              (Map.fromList runTimeVersions)
          mismatches = Map.mapMaybe id compares

      return
        $ getFirst
        $ foldMap
          (\p -> First $ (p,) <$> Map.lookup p mismatches)
          trackedPackages

-- | @makeGhcVersionChecker libdir@ returns a function to check the run-time
--   version of ghc against the compile-time version. It performs two checks:
--
--     1. It checks the version of the ghc installation given the run-time libdir
--        In some platforms, like Nix, the libdir is not fixed at compile-time
--
--     2. It compares the version of the 'ghc' package, if found at run-time.
--        If not, it compares the 'abi' of the 'base' package.
--
--    > ghcChecker :: IO(Ghc (String -> PackageCheck))
--    > ghcChecker = $$(makeGhcVersionChecker (pure $ Just GHC.Paths.libdir))
--    >
--    > checkGhcVersion :: IO ()
--    > checkGhcVersion = do
--    >     InstallationChecked ghcLibVersionChecker <- ghcChecker runTimeLibdir
--    >     res <- runGhc (Just runTimeLibdir) $ do
--    >              setupGhcApi
--    >              Right Nothing <- gtry ghcLibVersionChecker
--    >              doSomethingInteresting

makeGhcVersionChecker :: IO FilePath -> TExpQ GhcVersionChecker
makeGhcVersionChecker getLibdir = do
  libdir <- runIO getLibdir
  compileTimeVersions <-
    runIO
      $ runGhcPkg libdir
      $ collectPackageVersions trackedPackages
  [||checkGhcVersion trackedPackages $$(liftTyped compileTimeVersions)||]
  where
    trackedPackages = ["ghc", "base"]

runGhcPkg :: FilePath -> Ghc a -> IO a
runGhcPkg libdir action = runGhc (Just libdir) $ do
  -- initialize the Ghc session
  -- there's probably a better way to do this.
  dflags <- getSessionDynFlags
  _ <- setSessionDynFlags dflags
  action

-- | A GHC version retrieved from the ghc installation in the given libdir
ghcRunTimeVersion :: String -> IO Version
ghcRunTimeVersion libdir = do
    let guesses = guessExecutablePathFromLibdir libdir
    validGuesses <- filterM doesFileExist $ NonEmpty.toList guesses
    case validGuesses of
        firstGuess:_ -> getGhcVersion firstGuess
        [] -> fail $ "Unable to find the GHC executable for libdir: " <> libdir