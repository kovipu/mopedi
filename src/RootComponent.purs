module Mopedi.RootComponent where

import Prelude

import Mopedi.AppM (class LogMessages, logMessage, class WeeChat, initConnection, sendMessage)
import Mopedi.Store (Action(..), Store) as Store

import Control.Monad.Trans.Class (lift)
import Data.Array (length)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Store.Monad (class MonadStore, updateStore, getStore)

type State = { count :: Int }

data Query a = ReceiveMessage String a

newtype Message = Message String

data Action = Increment | Initialize | WebSocketMessage ArrayBuffer

component
  :: forall i m
   . LogMessages m
  => WeeChat m
  => MonadStore Store.Action Store.Store m
  => H.Component Query i Message m
component = do
  H.mkComponent
    { initialState: \_ -> { count: 0 }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div_
    [ HH.p_
        [ HH.text $ "You clicked " <> show state.count <> " times" ]
    , HH.button
        [ HE.onClick \_ -> Increment ]
        [ HH.text "Click me" ]
    , HH.button
        [ HE.onClick \_ -> Initialize ]
        [ HH.text "Connect" ]
    ]

handleAction
  :: forall m
   . LogMessages m
  => WeeChat m
  => MonadStore Store.Action Store.Store m
  => Action
  -> H.HalogenM State Action () Message m Unit
handleAction = case _ of
  Increment -> do
    logMessage "increment"

    -- TODO: remove debugging code.
    lift $ sendMessage $ "(hdata_buffers) hdata buffer:gui_buffers(*) number,full_name,short_name\n"
    { messages  } <- getStore
    
    logMessage $ show $ length messages

    H.modify_ \st -> st { count = st.count + 1 }

  Initialize -> do
    emitter <- lift $ initConnection "ws://localhost:8001/weechat" "test"
    H.subscribe' $ const $ map WebSocketMessage emitter
    pure unit

  WebSocketMessage msg ->
    updateStore $ Store.NewMessage msg

