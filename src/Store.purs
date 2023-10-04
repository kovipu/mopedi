module Mopedi.Store where

import Prelude

import Web.Socket.WebSocket (WebSocket)

data ConnectionState
  = Disconnected
  | Connecting
  | Connected WebSocket
  | Error
