
{- | Discover the GHC version via the package database. Requirements:

     * the package database must be compatible, which is usually not the case
       across major ghc versions.

     * the 'ghc' package is registered, which is not always the case.
 -}
module GHC.Check.PackageDb where

import           Control.Monad.Trans.Class as Monad (MonadTrans (lift))
import           Data.Maybe                (fromMaybe)
import           Data.String               (IsString (fromString))
import           Data.Version              (Version)
import           GHC                       (setSessionDynFlags, runGhc, getSessionDynFlags, Ghc)
import           GHC.Exts                  (IsList (fromList), toList)
import           Maybes                    (MaybeT (MaybeT), runMaybeT)
import           Module                    (componentIdToInstalledUnitId)
import           PackageConfig             (PackageName (PackageName))
import           Packages                  (lookupInstalledPackage, lookupPackageName)
import           Packages                  (InstalledPackageInfo (packageVersion))

-- | @getPackageVersion p@ returns the version of package @p@ in
--     the package database
getPackageVersion :: String -> Ghc (Maybe Version)
getPackageVersion packageName = runMaybeT $ do
    dflags <- Monad.lift getSessionDynFlags
    component <- MaybeT $ return $ lookupPackageName dflags $ PackageName $ fromString packageName
    p <- MaybeT $ return $ lookupInstalledPackage dflags (componentIdToInstalledUnitId component)
    return $ packageVersion p

-- | @getPackageVersionIO ghc-libdir p@ returns the version of package @p@
--   in the default package database
getPackageVersionIO :: FilePath -> String -> IO (Maybe Version)
getPackageVersionIO libdir package = runGhc (Just libdir) $ do
    -- initialize the Ghc session
    -- there's probably a beteter way to do this.
    dflags <- getSessionDynFlags
    _ <- setSessionDynFlags dflags

    getPackageVersion package