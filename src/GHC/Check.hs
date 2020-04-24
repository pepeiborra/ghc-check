{-# LANGUAGE TemplateHaskell #-}
module GHC.Check
( VersionCheck(..)
, checkGhcVersion
, compileTimeVersion
, ghcCompileTimeVersion
, runTimeVersion
) where

import           Data.Version               (Version)
import           GHC
import           GHC.Check.Internal

-- | Returns the compile-time version of the @ghc@ package.
--   Uses 'guessLibdir' to find the GHC lib dir
ghcCompileTimeVersion :: Version
ghcCompileTimeVersion = $$(compileTimeVersion guessLibdir "ghc")

-- | Returns the run-time version of the @ghc@ package in the package database
runTimeVersion :: Ghc (Maybe Version)
runTimeVersion = getGhcVersion

data VersionCheck
    = Match
    | Mismatch { compileTime :: Version
               , runTime     :: Maybe Version
               }
    deriving (Eq, Show)

-- | Checks if the run-time version of the @ghc@ package matches the compile-time version.
--   To be able to specify a custom libdir, inline this logic in your program.
checkGhcVersion :: Ghc VersionCheck
checkGhcVersion = do
    v <- getGhcVersion
    return $ if v == Just ghcCompileTimeVersion
        then GHC.Check.Match
        else Mismatch ghcCompileTimeVersion v
