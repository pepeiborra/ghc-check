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

getPackageVersion :: String -> Ghc (Maybe Version)
getPackageVersion packageName = runMaybeT $ do
    dflags <- Monad.lift getSessionDynFlags
    component <- MaybeT $ return $ lookupPackageName dflags $ PackageName $ fromString packageName
    p <- MaybeT $ return $ lookupInstalledPackage dflags (componentIdToInstalledUnitId component)
    return $ packageVersion p

getGhcVersion :: Ghc (Maybe Version)
getGhcVersion = getPackageVersion "ghc"

getGhcVersionIO :: FilePath -> IO Version
getGhcVersionIO libdir = runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    _ <- setSessionDynFlags dflags
    ghcV <- getGhcVersion
    case ghcV of
        Just v  -> return v
        Nothing -> error "Cannot get version of ghc package"

-- | Returns the compile-time version of the 'ghc' package given the GHC libdir
compileTimeVersionFromLibdir :: IO FilePath -> TExpQ Version
compileTimeVersionFromLibdir getLibdir = do
    ver <- runIO $ do
        libdir <- getLibdir
        v <- getGhcVersionIO libdir
        return (toList v)
    verLifted <- TH.lift (toList ver)
    [|| fromList $$(pure $ TExp verLifted) ||]
