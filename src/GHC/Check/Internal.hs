{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module GHC.Check.Internal where

import           Control.Applicative
import           Control.Monad             (join)
import           Control.Monad.Trans.Class as Monad (MonadTrans (lift))
import           Data.Maybe                (fromMaybe)
import           Data.String               (IsString (fromString))
import           Data.Version              (Version, makeVersion)
import           GHC
import           GHC.Exts                   (IsList (fromList), toList)
import           GHC.Paths
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax as TH (TExp(TExp), lift)
import           Maybes                    (MaybeT (MaybeT), runMaybeT)
import           Module                    (componentIdToInstalledUnitId)
import           PackageConfig             (PackageName (PackageName))
import           Packages                  (initPackages,
                                            lookupInstalledPackage,
                                            lookupPackage, lookupPackageName)
import           Packages                  (InstalledPackageInfo (packageVersion))
import           System.Environment        (lookupEnv)

-- Set the GHC libdir to the nix libdir if it's present.
guessLibdir :: IO FilePath
guessLibdir = fromMaybe GHC.Paths.libdir <$> lookupEnv "NIX_GHC_LIBDIR"

getPackageVersion :: String -> Ghc (Maybe Version)
getPackageVersion packageName = runMaybeT $ do
    dflags <- Monad.lift getSessionDynFlags
    component <- MaybeT $ return $ lookupPackageName dflags $ PackageName $ fromString packageName
    p <- MaybeT $ return $ lookupInstalledPackage dflags (componentIdToInstalledUnitId component)
    return $ packageVersion p

getGHCVersion :: Ghc (Maybe Version)
getGHCVersion = getPackageVersion "ghc"

getGHCVersionIO :: FilePath -> IO Version
getGHCVersionIO libdir = runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    _ <- setSessionDynFlags dflags
    ghcV <- getGHCVersion
    case ghcV of
        Just v  -> return v
        Nothing -> error "Cannot get version of ghc package"

-- | Returns the compile-time version of the 'ghc' package given the GHC libdir
compileTimeVersionFromLibDir :: IO FilePath -> TExpQ Version
compileTimeVersionFromLibDir getLibdir = do
    ver <- runIO $ do
        libdir <- getLibdir
        v <- getGHCVersionIO libdir
        return (toList v)
    verLifted <- TH.lift (toList ver)
    [|| fromList $$(pure $ TExp verLifted) ||]
