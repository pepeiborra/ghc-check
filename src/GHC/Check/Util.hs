{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP, TemplateHaskell #-}
module GHC.Check.Util (MyVersion(..), liftTyped, gcatchSafe) where

import           Control.Exception.Safe
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.Version ( Version, parseVersion )
import           GHC (Ghc, gcatch)
import           GHC.Exts                   (IsList (fromList), toList)
import           Language.Haskell.TH ( TExpQ )
import           Language.Haskell.TH.Syntax as TH
import qualified Text.Read as Read

-- | A wrapper around 'Version' with TH lifting
newtype MyVersion = MyVersion Version
  deriving (Eq, IsList, Show)

instance Lift MyVersion where
#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped = liftMyVersion
#endif
    lift = unTypeQ . liftMyVersion

instance Read MyVersion where
  readPrec = Read.lift $ MyVersion <$> parseVersion

liftMyVersion :: MyVersion -> TExpQ MyVersion
liftMyVersion ver = do
    verLifted <- TH.lift (toList ver)
    [|| fromList $$(pure $ TExp verLifted) ||]

#if !MIN_VERSION_template_haskell(2,16,0)
liftTyped :: Lift a => a -> TExpQ a
liftTyped = unsafeTExpCoerce . lift
#endif

gcatchSafe :: forall e a . Exception e => Ghc a -> (e -> Ghc a) -> Ghc a
gcatchSafe act h = act `gcatch` rethrowAsyncExceptions
  where
      rethrowAsyncExceptions :: e -> Ghc a
      rethrowAsyncExceptions e
        | isAsyncException e = liftIO . throwIO $ e
        | otherwise = h e
