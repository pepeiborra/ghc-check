{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP, TemplateHaskell #-}
module GHC.Check.Util (MyVersion(..), liftTyped) where

import           Data.Maybe
import           Data.Version
import           GHC.Exts                   (IsList (fromList), toList)
import qualified GHC.Paths
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax as TH
import           System.Environment (lookupEnv)

-- | A wrapper around 'Version' with TH lifting
newtype MyVersion = MyVersion Version
  deriving (Eq, IsList, Show)

instance Lift MyVersion where
#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped = liftMyVersion
#endif
    lift = unTypeQ . liftMyVersion

liftMyVersion :: MyVersion -> TExpQ MyVersion
liftMyVersion ver = do
    verLifted <- TH.lift (toList ver)
    [|| fromList $$(pure $ TExp verLifted) ||]

#if !MIN_VERSION_template_haskell(2,16,0)
liftTyped :: Lift a => a -> TExpQ a
liftTyped = unsafeTExpCoerce . lift
#endif