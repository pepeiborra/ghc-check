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
  )
where

import Control.Monad.Trans.Class as Monad (MonadTrans (lift))
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import Data.Version (Version)
import GHC
  ( Ghc,
    getSessionDynFlags,
    runGhc,
    setSessionDynFlags,
  )
import GHC.Check.Util
import GHC.Exts (IsList (fromList), toList)
import Maybes (MaybeT (MaybeT), runMaybeT)
import Module (componentIdToInstalledUnitId)
import PackageConfig (PackageName (PackageName))
import Packages
  ( lookupInstalledPackage,
    lookupPackageName,
  )
import Packages (InstalledPackageInfo (..))
import Packages (PackageConfig)
import Language.Haskell.TH.Syntax (Lift)
import Language.Haskell.TH (TExpQ)

data PackageVersion
  = PackageVersion
      { myVersion :: !MyVersion,
        abi :: !String
      }
  deriving (Eq, Lift, Show)

version :: PackageVersion -> Version
version PackageVersion{ myVersion = MyVersion v} = v

-- | @getPackageVersion p@ returns the version of package @p@ in the package database
getPackageVersion :: String -> Ghc (Maybe PackageVersion)
getPackageVersion packageName = runMaybeT $ do
  dflags <- Monad.lift getSessionDynFlags
  component <- MaybeT $ return $ lookupPackageName dflags $ PackageName $ fromString packageName
  p <- MaybeT $ return $ lookupInstalledPackage dflags (componentIdToInstalledUnitId component)
  return $ mkPackageVersion p

mkPackageVersion :: PackageConfig -> PackageVersion
mkPackageVersion p = PackageVersion (MyVersion $ packageVersion p) (abiHash p)