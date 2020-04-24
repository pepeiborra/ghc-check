{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module GHC.Check.Internal where

import           Control.Monad.Trans.Class as Monad (MonadTrans (lift))
import           Data.Maybe                (fromMaybe)
import           Data.String               (IsString (fromString))
import           Data.Version              (Version)
import           GHC
import           GHC.Exts                   (IsList (fromList), toList)
import qualified GHC.Paths
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax as TH (TExp(TExp), lift)
import           Maybes                    (MaybeT (MaybeT), runMaybeT)
import           Module                    (componentIdToInstalledUnitId)
import           PackageConfig             (PackageName (PackageName))
import           Packages                  (lookupInstalledPackage, lookupPackageName)
import           Packages                  (InstalledPackageInfo (packageVersion))
import           System.Environment        (lookupEnv)

-- | Look up the GHC lib dir in the NIX environment, then in the 'GHC.Paths'
guessLibdir :: IO FilePath
guessLibdir = fromMaybe GHC.Paths.libdir <$> lookupEnv "NIX_GHC_LIBDIR"

-- | @getPackageVersion p@ returns the version of package @p@ in
--     the package database
getPackageVersion :: String -> Ghc (Maybe Version)
getPackageVersion packageName = runMaybeT $ do
    dflags <- Monad.lift getSessionDynFlags
    component <- MaybeT $ return $ lookupPackageName dflags $ PackageName $ fromString packageName
    p <- MaybeT $ return $ lookupInstalledPackage dflags (componentIdToInstalledUnitId component)
    return $ packageVersion p

-- | Returns the version of the @ghc@ package in the environment package database
getGhcVersion :: Ghc (Maybe Version)
getGhcVersion = getPackageVersion "ghc"

-- | @getPackageVersionIO ghc-libdir p@ returns the version of package @p@
--   in the default package database
getPackageVersionIO :: FilePath -> String -> IO Version
getPackageVersionIO libdir package = runGhc (Just libdir) $ do
    -- initialize the Ghc session
    -- there's probably a beteter way to do this.
    dflags <- getSessionDynFlags
    _ <- setSessionDynFlags dflags

    ver <- getPackageVersion package
    case ver of
        Just v  ->
            return v
        Nothing ->
            error $ "Cannot find " <> package <> " in the package database at " <> libdir

-- | @compileTimeVersion get-libdir p@ returns the version of package @p@ in
--   the compile-time package database.
compileTimeVersion :: IO FilePath -> String -> TExpQ Version
compileTimeVersion getLibdir package = do
    ver <- runIO $ do
        libdir <- getLibdir
        v <- getPackageVersionIO libdir package
        return (toList v)
    verLifted <- TH.lift (toList ver)
    [|| fromList $$(pure $ TExp verLifted) ||]
