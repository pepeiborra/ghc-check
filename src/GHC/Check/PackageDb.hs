{-# LANGUAGE DeriveLift #-}
-- | Discover the GHC version via the package database. Requirements:
--
--     * the package database must be compatible, which is usually not the case
--       across major ghc versions.
--
--     * the 'ghc' package is registered, which is not always the case.
module GHC.Check.PackageDb
  ( PackageVersion(abi), version,
    getPackageVersion,
    fromVersionString
   )
where

import Control.Monad.Trans.Class as Monad (MonadTrans (lift))
import Data.String (IsString (fromString))
import Data.Version (Version)
import GHC
  (pkgState,  Ghc,
    getSessionDynFlags,
  )
import GHC.Check.Util
import Maybes (MaybeT (MaybeT), runMaybeT)
import Module (componentIdToInstalledUnitId)
import PackageConfig (PackageName (PackageName))
import Packages
  (lookupPackage, explicitPackages,  lookupInstalledPackage,
    lookupPackageName
  )
import Packages (InstalledPackageInfo (..))
import Packages (PackageConfig)
import Language.Haskell.TH.Syntax (Lift)
import Data.Foldable (find)
import Packages (packageNameString)
import Control.Applicative (Alternative((<|>)))
import GHC.Stack (HasCallStack)

data PackageVersion
  = PackageVersion
      { myVersion :: !MyVersion,
        abi :: Maybe String
      }
  deriving (Eq, Lift, Show)

version :: PackageVersion -> Version
version PackageVersion{ myVersion = MyVersion v} = v

-- | @getPackageVersion p@ returns the version of package @p@ that will be used in the Ghc session.
getPackageVersion :: String -> Ghc (Maybe PackageVersion)
getPackageVersion pName = runMaybeT $ do
  dflags <- Monad.lift getSessionDynFlags
  let pkgst   = pkgState dflags
      depends = explicitPackages pkgst

  let explicit = do
        pkgs <- traverse (MaybeT . return . lookupPackage dflags) depends
        MaybeT $ return $ find (\p -> packageNameString p == pName ) pkgs

      notExplicit = do
        component <- MaybeT $ return $ lookupPackageName dflags $ PackageName $ fromString pName
        MaybeT $ return $ lookupInstalledPackage dflags (componentIdToInstalledUnitId component)

  p <- explicit <|> notExplicit

  return $ fromPackageConfig p

fromPackageConfig :: PackageConfig -> PackageVersion
fromPackageConfig p = PackageVersion (MyVersion $ packageVersion p) (Just $ abiHash p)

fromVersionString :: HasCallStack => String -> PackageVersion
fromVersionString v = PackageVersion (MyVersion $ read v) Nothing