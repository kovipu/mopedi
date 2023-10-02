module Mopedi.Store where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array ((:))
import Data.ArrayBuffer.Types (ArrayBuffer)
import Web.Socket.WebSocket (WebSocket)

type Store =
  { connection :: Maybe WebSocket
  , messages :: Array ArrayBuffer
  }

initialStore :: Store
initialStore =
  { connection: Nothing
  , messages: []
  }

data Action
  = NewConnection WebSocket
  | NewMessage ArrayBuffer

reduce :: Store -> Action -> Store
reduce store = case _ of
  NewConnection socket ->
    store { connection = Just socket }

  NewMessage msg ->
    store { messages = msg : store.messages }
