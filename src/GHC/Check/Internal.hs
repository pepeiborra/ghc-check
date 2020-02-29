{-# LANGUAGE OverloadedStrings #-}
module GHC.Check.Internal where

import Control.Applicative
import GHC
import GHC.Paths
import Data.Version (makeVersion, Version)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Packages (initPackages, lookupInstalledPackage, lookupPackageName, lookupPackage)
import PackageConfig (PackageName(PackageName))
import Maybes (runMaybeT, MaybeT(MaybeT))
import Module (componentIdToInstalledUnitId)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Packages (InstalledPackageInfo(packageVersion))
import Control.Monad (join)
import Data.String (IsString(fromString))

-- Set the GHC libdir to the nix libdir if it's present.
getLibdir :: IO FilePath
getLibdir = fromMaybe GHC.Paths.libdir <$> lookupEnv "NIX_GHC_LIBDIR"

getPackageVersion :: String -> Ghc (Maybe Version)
getPackageVersion packageName = runMaybeT $ do
    dflags <- lift getSessionDynFlags
    component <- MaybeT $ return $ lookupPackageName dflags $ PackageName $ fromString packageName
    p <- MaybeT $ return $ lookupInstalledPackage dflags (componentIdToInstalledUnitId component)
    return $ packageVersion p

getGHCVersion :: Ghc (Maybe Version)
getGHCVersion = getPackageVersion "ghc"

getGHCVersionIO :: IO Version
getGHCVersionIO = do
    libdir <- getLibdir
    runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        _ <- setSessionDynFlags dflags
        ghcV <- getGHCVersion
        case ghcV of
            Just v -> return v
            Nothing -> error "Cannot get version of ghc package"