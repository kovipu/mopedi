module Mopedi.AppM where

import Prelude

import Mopedi.Store (Store, Action, reduce)
import Control.Monad.Trans.Class (lift)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Console as Console
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen (HalogenM)
import Halogen.Store.Monad (class MonadStore, StoreT, runStoreT)
import Safe.Coerce (coerce)

-- A custom application monad that provides the capabilities we need.
newtype AppM a = AppM (StoreT Action Store Aff a)

runAppM :: forall q i o. Store -> H.Component q i o AppM -> Aff (H.Component q i o Aff)
runAppM store = runStoreT store reduce <<< coerce

-- To be a monad, a type must implement these type classes:
-- Functor, Apply, Applicative, Bind and Monad.
derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM

-- We can also derive MonadEffect and MonadAff because we used Aff.
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

-- We're using halogen-store, so we can also derive a MonadStore.
derive newtype instance monadStoreAppM :: MonadStore Action Store AppM

-- Capability type classes.
class Monad m <= LogMessages m where
  logMessage :: String -> m Unit

instance logMessagesHalogenM :: LogMessages m => LogMessages (HalogenM st act slots msg m) where
  logMessage = lift <<< logMessage

-- Concrete implementations for the capabilites.
instance logMessagesAppM :: LogMessages AppM where
  logMessage = liftEffect <<< Console.log
