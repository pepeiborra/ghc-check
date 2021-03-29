{-# LANGUAGE CPP          #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module GHC.Check
  ( -- * GHC version check
    makeGhcVersionChecker,
    GhcVersionChecker,
    InstallationCheck (..),
    PackageCheckResult (..),
    PackageCheck (..),

    -- ** Interpreting the results
    guessCompatibility,
    CompatibilityGuess (..),
    NotCompatibleReason(..),

    -- ** Exports for TH
    checkGhcVersion,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Exception
import Control.Monad (filterM, unless)
import Data.List (find)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Version (Version)
import GHC (Ghc)
import GHC.Check.Executable (getGhcVersion, guessExecutablePathFromLibdir)
import GHC.Check.PackageDb (PackageVersion (..), getPackageVersion, version)
import GHC.Check.Util (gcatchSafe, liftTyped)
import Language.Haskell.TH as TH (TExpQ, runIO)
import qualified Language.Haskell.TH as TH
import System.Directory (doesDirectoryExist, doesFileExist)

#if USE_PACKAGE_ABIS
import GHC (getSessionDynFlags, runGhc, setSessionDynFlags)
#else
import GHC.Check.PackageDb (fromVersionString)
#endif

-- | Given a run-time libdir, checks the ghc installation and returns
--   a 'Ghc' action to check the package database
type GhcVersionChecker = String -> IO InstallationCheck

data InstallationCheck
  = -- | The GHC installation looks fine. Further checks are needed for the package libraries.
    InstallationChecked
      { -- | The compile time version of GHC
        compileTime :: !Version,
        -- | The second stage of the GHC version check
        packageCheck :: Ghc PackageCheckResult
      }
  | -- | The libdir points to a different GHC version
    InstallationMismatch {libdir :: !String, compileTime, runTime :: !Version}
  | -- | The libdir does not exist
    InstallationNotFound {libdir :: !String}

data PackageCheckResult
  = -- | All the compile time packages tested match
    PackageCheckSuccess !(NonEmpty (String, PackageCheck))
  | -- | Found package mismatches
    PackageCheckFailure !(NonEmpty (String, PackageCheck))
  | -- | None of the compile time packages could be found
    PackageCheckInconclusive ![String]
    -- | An exception arised during the package check
  | PackageCheckError !SomeException

data PackageCheck
  = VersionMismatch {compileTime, runTime :: !Version}
    -- ^ Different versions
  | AbiMismatch {compileTimeAbi, runTimeAbi :: !String, compileTime :: !Version}
    -- ^ Same version but different abi
  | VersionMatch {packageVersion :: !PackageVersion}
    -- ^ Same version and abi
  deriving (Eq, Show)

isPackageCheckFailure VersionMatch {} = False
isPackageCheckFailure _ = True

comparePackageVersions :: PackageVersion -> PackageVersion -> PackageCheck
comparePackageVersions compile run
  | version compile /= version run =
    VersionMismatch (version compile) (version run)
  | Just abiCompile <- abi compile
  , Just abiRun <- abi run
  , abiCompile /= abiRun
  = AbiMismatch abiCompile abiRun (version compile)
  | otherwise
  = VersionMatch compile

collectPackageVersions :: [String] -> Ghc [(String, PackageVersion)]
collectPackageVersions =
  fmap catMaybes . mapM (\p -> fmap (p,) <$> getPackageVersion p)

-- | Checks if the run-time version of the @ghc@ package matches the given version.
checkGhcVersion ::
  [(String, PackageVersion)] ->
  GhcVersionChecker
checkGhcVersion compileTimeVersions runTimeLibdir = do
  let compileTimeVersionsMap = Map.fromList compileTimeVersions
      compileTime = version $ compileTimeVersionsMap Map.! "ghc"

  exists <- doesDirectoryExist runTimeLibdir

  if not exists
    then return $ InstallationNotFound runTimeLibdir
    else do
      runTime <- ghcRunTimeVersion runTimeLibdir

      return $
        if runTime /= compileTime
          then InstallationMismatch {libdir = runTimeLibdir, ..}
          else InstallationChecked compileTime
            $ flip gcatchSafe (pure . PackageCheckError)
            $ do
              runTimeVersions <- collectPackageVersions (map fst compileTimeVersions)
              let compares =
                    Map.intersectionWith
                      comparePackageVersions
                      compileTimeVersionsMap
                      (Map.fromList runTimeVersions)
                  failure = PackageCheckFailure <$> nonEmpty (Map.toList $ Map.filter isPackageCheckFailure compares)
                  success = PackageCheckSuccess <$> nonEmpty (Map.toList compares)
                  inconclusive = PackageCheckInconclusive (map fst compileTimeVersions)

              return $ fromMaybe inconclusive (failure <|> success)

-- | @makeGhcVersionChecker libdir@ returns a function to check the run-time
--   version of GHC against the compile-time version. It performs two checks:
--
--     1. It checks the version of the GHC installation given the run-time libdir
--        In some platforms, like Nix, the libdir is not fixed at compile-time
--
--     2. It compares the version of the @ghc@ package, if found at run-time.
--        If not, it compares the abi of the @base@ package.
--
--    > ghcChecker :: IO(Ghc (String -> PackageCheck))
--    > ghcChecker = $$(makeGhcVersionChecker (pure $ Just GHC.Paths.libdir))
--    >
--    > checkGhcVersion :: IO ()
--    > checkGhcVersion = do
--    >     InstallationChecked packageCheck <- ghcChecker runTimeLibdir
--    >     res <- runGhc (Just runTimeLibdir) $ do
--    >              setupGhcApi
--    >              result <- packageCheck
--    >              case guessCompatibility result of ...
makeGhcVersionChecker :: IO FilePath -> TExpQ GhcVersionChecker
makeGhcVersionChecker getLibdir = do
  compileTimeVersions <- runIO $ compileTimeVersions getLibdir
#if MIN_VERSION_template_haskell(2,17,0)
  TH.examineCode [||checkGhcVersion $$(liftTyped compileTimeVersions)||]
#else
  [||checkGhcVersion $$(liftTyped compileTimeVersions)||]
#endif

compileTimeVersions :: IO FilePath -> IO [(String, PackageVersion)]
compileTimeVersions getLibdir = do
#if USE_PACKAGE_ABIS
  libdir <- getLibdir
  libdirExists <- doesDirectoryExist libdir
  unless libdirExists
    $ error
    $ "I could not find a GHC installation at " <> libdir
      <> ". Please do a clean rebuild and/or reinstall GHC."
  runGhcPkg libdir $ collectPackageVersions ["ghc", "base"]

runGhcPkg :: FilePath -> Ghc a -> IO a
runGhcPkg libdir action = runGhc (Just libdir) $ do
  -- initialize the Ghc session
  -- there's probably a better way to do this.
  dflags <- getSessionDynFlags
  _ <- setSessionDynFlags dflags
  action
#else
  return
    [ ("ghc", fromVersionString VERSION_ghc)
    , ("base", fromVersionString VERSION_base)
    ]
#endif

-- | A GHC version retrieved from the GHC installation in the given libdir
ghcRunTimeVersion :: String -> IO Version
ghcRunTimeVersion libdir = do
  let guesses = guessExecutablePathFromLibdir libdir
  validGuesses <- filterM doesFileExist $ NonEmpty.toList guesses
  case validGuesses of
    firstGuess : _ -> getGhcVersion firstGuess
    [] -> fail $ "Unable to find the GHC executable for libdir: " <> libdir

--------------------------------------------------------------------------------

-- | The result of interpreting a 'PackageCheckResult'
data CompatibilityGuess
  = ProbablyCompatible {warning :: Maybe String}
  | NotCompatible {reason :: !NotCompatibleReason}
  deriving (Eq, Show)

data NotCompatibleReason
  = PackageVersionMismatch
      { compileTime :: !Version,
        runTime :: !Version,
        packageName :: !String
      }
  | BasePackageAbiMismatch
      { compileTimeAbi :: !String,
        runTimeAbi :: !String,
        compileTime :: !Version
      }
  deriving (Eq, Show)

-- | Interpret a 'PackageCheckResult' into a yes/no GHC compatibility answer
guessCompatibility :: PackageCheckResult -> CompatibilityGuess
guessCompatibility result = case result of
  PackageCheckFailure evidence
    | Just problem <- findInterestingProblem evidence -> do
      case problem of
        (packageName, VersionMismatch {..}) ->
          NotCompatible PackageVersionMismatch {..}
        ("base", AbiMismatch {..}) ->
          NotCompatible BasePackageAbiMismatch {..}
        (_, VersionMatch {}) ->
          ProbablyCompatible Nothing
    | otherwise ->
      ProbablyCompatible Nothing
  PackageCheckInconclusive attempts ->
    ProbablyCompatible $ Just $
      "unable to validate GHC version. Could not find any run-time packages to test: "
        <> show attempts
  PackageCheckError err ->
    ProbablyCompatible $ Just $ "Warning: unable to validate GHC version: " <> show err
  PackageCheckSuccess !_evidence ->
    ProbablyCompatible Nothing

findInterestingProblem :: NonEmpty (String, PackageCheck) -> Maybe (String, PackageCheck)
findInterestingProblem evidence = find isInterestingProblem evidence
  where
    ghcVersionMatches = any isGhcVersionMatchEvidence evidence
    isInterestingProblem (_, VersionMismatch {}) = True
    isInterestingProblem (_, AbiMismatch {}) =
      -- The package version matches, but the abi does not.
      -- This can happen if we have been built by:
      --   1) a different version of ghc, or
      --   2) a different build tool
      -- We tolerate only if there is evidence that it's not case 1
      not ghcVersionMatches

    isInterestingProblem _ = False
    isGhcVersionMatchEvidence ("ghc", VersionMatch {}) = True
    isGhcVersionMatchEvidence ("ghc", AbiMismatch {}) =
      -- We assume that an abi mismatch implies a version match,
      -- otherwise the library would have reported version mismatch
      -- rather than abi mismatch.
      True
    isGhcVersionMatchEvidence _ = False
