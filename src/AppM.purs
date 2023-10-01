module Mopedi.AppM where

import Prelude

import Mopedi.Store (Store, Action(..), reduce)

import Control.Monad.Trans.Class (lift)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), fromJust)
import Data.Time.Duration (Milliseconds(..))
import Partial.Unsafe (unsafePartial)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff (makeAff, error, effectCanceler, delay) as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Console as Console
import Effect.Class (class MonadEffect, liftEffect)
import Foreign (unsafeFromForeign)
import Halogen (Component) as H
import Halogen.Query.Event (eventListener) as H
import Halogen.Subscription (Emitter)
import Halogen (HalogenM)
import Halogen.Store.Monad (class MonadStore, StoreT, runStoreT, updateStore, getStore)
import Safe.Coerce (coerce)
import Web.Event.Event (Event)
import Web.Event.EventTarget as Event
import Web.Socket.Event.EventTypes (onMessage, onOpen) as WebSocket
import Web.Socket.Event.MessageEvent as WebSocket.MessageEvent
import Web.Socket.BinaryType (BinaryType(ArrayBuffer))
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket (create, sendString, toEventTarget, setBinaryType) as WebSocket

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

-- We're using halogen-store, so we can also derive MonadStore.
derive newtype instance monadStoreAppM :: MonadStore Action Store AppM

-- Capability type classes.
class Monad m <= LogMessages m where
  logMessage :: String -> m Unit

instance logMessagesHalogenM :: LogMessages m => LogMessages (HalogenM st act slots msg m) where
  logMessage = lift <<< logMessage

class Monad m <= WeeChat m where
  initConnection :: String -> String -> m (Emitter ArrayBuffer)

  sendMessage :: String -> m Unit

-- Concrete implementations for the capabilites.
instance logMessagesAppM :: LogMessages AppM where
  logMessage = liftEffect <<< Console.log

instance weeChatAppM :: WeeChat AppM where
  initConnection :: String -> String -> AppM (Emitter ArrayBuffer)
  initConnection addr pw = do
    -- initialize web socket and authenticate.
    socket <- liftEffect $ WebSocket.create addr []
    liftEffect $ WebSocket.setBinaryType socket ArrayBuffer

    let
      target :: Event.EventTarget
      target = WebSocket.toEventTarget socket

    -- Wait for socket to open before doing anything.
    liftAff $ Aff.makeAff \callback -> do
      listener <- Event.eventListener \_ ->
        callback $ Right unit
      Event.addEventListener WebSocket.onOpen listener false target

      pure $ Aff.effectCanceler do
        Event.removeEventListener WebSocket.onOpen listener false target

    liftEffect $ WebSocket.sendString socket $ "init password=" <> pw <> ",compression=off\n"
    updateStore $ NewConnection socket

    let
      handleEvent :: Event -> Maybe ArrayBuffer
      handleEvent event = do
        messageEvent <- WebSocket.MessageEvent.fromEvent event
        let
          foreignData = WebSocket.MessageEvent.data_ messageEvent

          msg :: ArrayBuffer
          msg = unsafeFromForeign foreignData
        pure msg

    pure $ H.eventListener WebSocket.onMessage target handleEvent

  sendMessage :: String -> AppM Unit
  sendMessage msg = do
    { connection } <- getStore
    case connection of
      Nothing -> logMessage "Error! No WebSocket connection."
      Just socket -> liftEffect $ WebSocket.sendString socket msg

