module Mopedi.AppM where

import Prelude

import Mopedi.Store (Store, Action(..), reduce)

import Control.Monad.Trans.Class (lift)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Maybe (Maybe(..))
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

data WebSocketEvent
  = WebSocketMessage ArrayBuffer
  | WebSocketOpen Event
  | WebSocketClose Event
  | WebSocketError Event

class Monad m <= WeeChat m where
  initConnection :: String -> m (HS.Emitter WebSocketEvent)

  authenticate :: String -> m Unit

  requestBuffers :: m Unit

  requestHistory :: m Unit

-- Concrete implementations for the capabilites.
instance logMessagesAppM :: LogMessages AppM where
  logMessage = liftEffect <<< Console.log

instance weeChatAppM :: WeeChat AppM where
  initConnection :: String -> AppM (HS.Emitter WebSocketEvent)
  initConnection addr = do
    -- initialize web socket and authenticate.
    socket <- liftEffect $ WebSocket.create addr []
    liftEffect $ WebSocket.setBinaryType socket ArrayBuffer

    { emitter, listener } :: HS.SubscribeIO WebSocketEvent <- liftEffect HS.create

    liftEffect $ do
      void $ HS.subscribe (makeMessageEmitter socket) \message ->
        HS.notify listener $ WebSocketMessage message

      void $ HS.subscribe (makeOpenEmitter socket) \openEvent ->
        HS.notify listener $ WebSocketOpen openEvent

      void $ HS.subscribe (makeCloseEmitter socket) \closeInfo ->
        HS.notify listener $ WebSocketClose closeInfo

      void $ HS.subscribe (makeErrorEmitter socket) \err ->
        HS.notify listener $ WebSocketError err

    updateStore $ NewConnection socket

    pure emitter

    where
    makeMessageEmitter :: WebSocket -> HS.Emitter ArrayBuffer
    makeMessageEmitter socket =
      makeWebSocketEmitter WebSocket.onMessage socket readMessageEvent

    readMessageEvent :: Event -> Maybe ArrayBuffer
    readMessageEvent event = do
      messageEvent <- WebSocket.MessageEvent.fromEvent event
      let
        msg :: ArrayBuffer
        msg = WebSocket.MessageEvent.data_ messageEvent
          # unsafeFromForeign
      pure msg

    makeOpenEmitter :: WebSocket -> HS.Emitter Event
    makeOpenEmitter socket =
      makeWebSocketEmitter WebSocket.onOpen socket Just

    makeCloseEmitter :: WebSocket -> HS.Emitter Event
    makeCloseEmitter socket =
      makeWebSocketEmitter WebSocket.onClose socket Just

    makeErrorEmitter :: WebSocket -> HS.Emitter Event
    makeErrorEmitter socket =
      makeWebSocketEmitter WebSocket.onError socket Just

    makeWebSocketEmitter :: forall a. EventType -> WebSocket -> (Event -> Maybe a) -> HS.Emitter a
    makeWebSocketEmitter eventType socket handler =
      HQE.eventListener eventType (WebSocket.toEventTarget socket) handler

  authenticate :: String -> AppM Unit
  authenticate password =
    sendMessage $ "init password=" <> password <> ",compression=off\n"

  requestBuffers :: AppM Unit
  requestBuffers =
    sendMessage "(buffers) hdata buffer:gui_buffers(*) number,full_name,short_name\n"

  requestHistory :: AppM Unit
  requestHistory =
    sendMessage "(history) hdata buffer:gui_buffers(*)/own_lines/first_line(*)/data message,buffer,date,prefix\n"

sendMessage :: String -> AppM Unit
sendMessage msg = do
  { connection } <- getStore
  case connection of
    Nothing -> logMessage "Error! No WebSocket connection."
    Just socket -> liftEffect $ WebSocket.sendString socket msg

