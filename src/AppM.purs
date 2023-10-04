module Mopedi.AppM where

import Prelude

import Mopedi.Store (ConnectionState(..))

import Control.Monad.Trans.Class (lift)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Console as Console
import Effect.Class (class MonadEffect, liftEffect)
import Foreign (unsafeFromForeign)
import Halogen as H
import Halogen.Subscription as HS
import Halogen.Query.Event as HQE
import Halogen (HalogenM)
import Halogen.Store.Monad (class MonadStore, StoreT, runStoreT, updateStore, getStore)
import Safe.Coerce (coerce)
import Web.Event.Event (Event, EventType)
import Web.Socket.Event.EventTypes (onMessage, onOpen, onClose, onError) as WebSocket
import Web.Socket.Event.MessageEvent as WebSocket.MessageEvent
import Web.Socket.BinaryType (BinaryType(ArrayBuffer))
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket (create, sendString, toEventTarget, setBinaryType) as WebSocket

-- A custom application monad that provides the capabilities we need.
newtype AppM a = AppM (StoreT Unit Unit Aff a)

runAppM :: forall q i o. Unit -> H.Component q i o AppM -> Aff (H.Component q i o Aff)
runAppM store = runStoreT store const <<< coerce

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
derive newtype instance monadStoreAppM :: MonadStore Unit Unit AppM

-- Capability type classes.
class Monad m <= LogMessages m where
  logMessage :: String -> m Unit

instance logMessagesHalogenM :: LogMessages m => LogMessages (HalogenM st act slots msg m) where
  logMessage = lift <<< logMessage

data WebSocketEvent
  = WebSocketMessage ArrayBuffer
  | WebSocketOpen Event
  | WebSocketClose Event
  | WebSocketError Event

class Monad m <= WeeChat m where
  initConnection :: String -> m (Tuple WebSocket (HS.Emitter WebSocketEvent))

  authenticate :: WebSocket -> String -> m Unit

  requestBuffers :: WebSocket -> m Unit

  requestHistory :: WebSocket -> m Unit

-- Concrete implementations for the capabilites.
instance logMessagesAppM :: LogMessages AppM where
  logMessage = liftEffect <<< Console.log

instance weeChatAppM :: WeeChat AppM where
  initConnection :: String -> AppM (Tuple WebSocket (HS.Emitter WebSocketEvent))
  initConnection addr = do
    socket <- liftEffect $ WebSocket.create addr []
    liftEffect $ WebSocket.setBinaryType socket ArrayBuffer

    { emitter, listener } :: HS.SubscribeIO WebSocketEvent <- liftEffect HS.create

    let
      makeWebSocketEmitter :: forall a. EventType -> WebSocket -> (Event -> Maybe a) -> HS.Emitter a
      makeWebSocketEmitter eventType socket' handler =
        HQE.eventListener eventType (WebSocket.toEventTarget socket') handler

      subscribe :: forall a. EventType -> (a -> WebSocketEvent) -> (Event -> Maybe a) -> Effect Unit
      subscribe eventType toSocketEvent readEvent =
        void
          $ HS.subscribe
              (makeWebSocketEmitter eventType socket readEvent)
          $ HS.notify listener <<< toSocketEvent

      readMessageEvent :: Event -> Maybe ArrayBuffer
      readMessageEvent event = do
        messageEvent <- WebSocket.MessageEvent.fromEvent event
        let
          msg :: ArrayBuffer
          msg = WebSocket.MessageEvent.data_ messageEvent
            # unsafeFromForeign
        pure msg

    liftEffect $ do
      subscribe WebSocket.onOpen WebSocketOpen Just
      subscribe WebSocket.onClose WebSocketClose Just
      subscribe WebSocket.onError WebSocketError Just
      subscribe WebSocket.onMessage WebSocketMessage readMessageEvent

    pure $ socket /\ emitter

  authenticate :: WebSocket -> String -> AppM Unit
  authenticate socket password =
    WebSocket.sendString socket ("init password=" <> password <> ",compression=off\n")
      # liftEffect

  requestBuffers :: WebSocket -> AppM Unit
  requestBuffers socket =
    WebSocket.sendString socket "(buffers) hdata buffer:gui_buffers(*) number,full_name,short_name\n"
      # liftEffect

  requestHistory :: WebSocket -> AppM Unit
  requestHistory socket =
    WebSocket.sendString socket "(history) hdata buffer:gui_buffers(*)/own_lines/first_line(*)/data message,buffer,date,prefix\n"
      # liftEffect

