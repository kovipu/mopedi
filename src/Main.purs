module Main where

import Prelude

import Mopedi.RootComponent (component, Message(..), Query(..))
import Mopedi.AppM (runAppM)
import Mopedi.Store (Store, initialStore)

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
  HA.runHalogenAff do
    body <- HA.awaitBody

    rootComponent <- runAppM initialStore component

    io <- runUI rootComponent unit body

    pure unit
