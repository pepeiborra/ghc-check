{-# LANGUAGE TemplateHaskell #-}
module GHC.Check
( VersionCheck(..)
, checkGhcVersion
, compileTimeVersion
, compileTimeVersionFromLibdir
, runTimeVersion
) where

import           Data.Version               (Version)
import           GHC
import           GHC.Check.Internal

-- | Returns the compile-time version of the 'ghc' package.
--   Uses 'guessLibdir' to find the GHC lib dir
compileTimeVersion :: Version
compileTimeVersion = $$(compileTimeVersionFromLibdir guessLibdir)

-- | Returns the run-time version of the 'ghc' package by looking up in the package database
runTimeVersion :: Ghc (Maybe Version)
runTimeVersion = getGhcVersion

data VersionCheck
    = Match
    | Mismatch { compileTime :: Version
               , runTime     :: Maybe Version
               }
    deriving (Eq, Show)

-- | Returns 'True' if the run-time version of the 'ghc' package matches the compile-time version
--   For using your own GHC libdir at compile time, inline this logic in your program
checkGhcVersion :: Ghc VersionCheck
checkGhcVersion = do
    v <- getGhcVersion
    return $ if v == Just compileTimeVersion
        then GHC.Check.Match
        else Mismatch compileTimeVersion v
