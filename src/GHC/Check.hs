{-# LANGUAGE TemplateHaskell #-}
module GHC.Check
( checkGhcVersion
, compileTimeVersion
, runTimeVersion
, guessLibdir
) where

import           Data.Version               (Version)
import           GHC
import           GHC.Check.Internal

-- | Returns the compile-time version of the 'ghc' package
compileTimeVersion :: Version
compileTimeVersion = $$(compileTimeVersionFromLibDir guessLibdir)

-- | Returns the run-time version of the 'ghc' package by looking up in the package database
runTimeVersion :: Ghc (Maybe Version)
runTimeVersion = getGHCVersion

-- | Returns 'True' if the run-time version of the 'ghc' package matches the compile-time version
checkGhcVersion :: Ghc Bool
checkGhcVersion = do
    v <- getPackageVersion "ghc"
    return (Just compileTimeVersion == v)
