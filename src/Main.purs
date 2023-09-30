module Main where

import Prelude

import Mopedi.RootComponent (component, Message(..), Query(..))
import Mopedi.AppM (runAppM)
import Mopedi.Store (Store)

import Control.Coroutine (Consumer, Producer, ($$))
import Control.Coroutine as CR
import Control.Coroutine.Aff (emit, Emitter)
import Control.Coroutine.Aff (produce) as Co
import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (F, Foreign, readString, unsafeToForeign)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (Event)
import Web.Event.EventTarget as Event
import Web.Socket.Event.EventTypes (onMessage) as WebSocket
import Web.Socket.Event.MessageEvent as WebSocket.MessageEvent
import Web.Socket.BinaryType (BinaryType(ArrayBuffer))
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket (create, sendString, toEventTarget, setBinaryType) as WebSocket

main :: Effect Unit
main = do
  let
    baseUrl :: String
    baseUrl = "ws://localhost:8001/weechat"

  connection <- WebSocket.create baseUrl []
  WebSocket.setBinaryType connection ArrayBuffer

  HA.runHalogenAff do
    body <- HA.awaitBody

    let
      initialStore :: Store
      initialStore = { baseUrl }

    rootComponent <- runAppM initialStore component

    io <- runUI rootComponent unit body

    -- subscribe to all output messages from our component
    _ <- H.liftEffect $ HS.subscribe io.messages $ wsSender connection

    -- Connecting the consumer to the producer initializes both,
    -- feeding queries back to our component as messages are received.
    CR.runProcess (wsProducer connection $$ wsConsumer io.query)

-- A producer coroutine that emits messages that arrive from the websocket.
wsProducer :: WebSocket -> Producer String Aff Unit
wsProducer socket = Co.produce \emitter -> do
  listener :: Event.EventListener <-
    Event.eventListener $ listen emitter

  Event.addEventListener
    WebSocket.onMessage
    listener
    false
    (WebSocket.toEventTarget socket)

  where
  listen :: Emitter Effect String Unit -> Event -> Effect Unit
  listen emitter ev = do
    for_ (WebSocket.MessageEvent.fromEvent ev) \msgEvent ->
      for_ (readHelper readString (WebSocket.MessageEvent.data_ msgEvent)) \msg ->
        emit emitter msg

  readHelper :: forall a b. (Foreign -> F a) -> b -> Maybe a
  readHelper read =
    hush <<< runExcept <<< read <<< unsafeToForeign

-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends `ReceiveMessage` queries in when it receives inputs from the
-- producer.
wsConsumer :: (forall a. Query a -> Aff (Maybe a)) -> Consumer String Aff Unit
wsConsumer query = CR.consumer \msg -> do
  void $ query $ H.mkTell $ ReceiveMessage msg
  pure Nothing

-- A handler for messages from our component IO that sends them to the server
-- using the websocket
wsSender :: WebSocket -> Message -> Effect Unit
wsSender socket (Message msg) = WebSocket.sendString socket msg

