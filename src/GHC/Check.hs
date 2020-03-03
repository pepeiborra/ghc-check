{-# LANGUAGE TemplateHaskell #-}
module GHC.Check (checkGhcVersion, compileTimeVersion, runTimeVersion) where

import           Data.Version               (Version)
import           GHC
import           GHC.Check.Internal
import           GHC.Exts                   (IsList (fromList), toList)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (lift)

-- | Returns the compile-time version of the 'ghc' package
compileTimeVersion :: Version
compileTimeVersion = fromList $(lift =<< toList <$> runIO getGHCVersionIO)

-- | Returns the run-time version of the 'ghc' package by looking up in the package database
runTimeVersion :: Ghc (Maybe Version)
runTimeVersion = getGHCVersion

-- | Returns 'True' if the run-time version of the 'ghc' package matches the compile-time version
checkGhcVersion :: Ghc Bool
checkGhcVersion = do
    v <- getPackageVersion "ghc"
    return (Just compileTimeVersion == v)
