{-# LANGUAGE TemplateHaskell #-}
module GHC.Check.Util where


import           Data.Maybe
import           Data.Version
import           GHC.Exts                   (IsList (fromList), toList)
import qualified GHC.Paths
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax as TH
import           System.Environment (lookupEnv)

-- | Look up the GHC lib dir in the NIX environment, then in the 'GHC.Paths'
guessLibdir :: IO FilePath
guessLibdir = fromMaybe GHC.Paths.libdir <$> lookupEnv "NIX_GHC_LIBDIR"

liftVersion :: Version -> TExpQ Version
liftVersion ver = do
    verLifted <- TH.lift (toList ver)
    [|| fromList $$(pure $ TExp verLifted) ||]
