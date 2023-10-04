module Mopedi.RootComponent where

import Prelude

import Mopedi.AppM (class LogMessages, logMessage, WebSocketEvent(..), class WeeChat, initConnection, authenticate, requestBuffers, requestHistory)
import Mopedi.Store (Action(..), Store) as Store

import Control.Monad.Trans.Class (lift)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Store.Monad (class MonadStore, updateStore)

type State = { count :: Int }

data Action = Increment | Initialize | ReceiveEvent WebSocketEvent

component
  :: forall i q o m
   . LogMessages m
  => WeeChat m
  => MonadStore Store.Action Store.Store m
  => H.Component q i o m
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
  :: forall o m
   . LogMessages m
  => WeeChat m
  => MonadStore Store.Action Store.Store m
  => Action
  -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Increment -> do
    logMessage "increment"
    H.modify_ \st -> st { count = st.count + 1 }

  Initialize -> do
    emitter <- lift $ initConnection "ws://localhost:8001/weechat"
    void $ H.subscribe (ReceiveEvent <$> emitter)

  ReceiveEvent event ->
    case event of
      WebSocketMessage msg ->
        updateStore $ Store.NewMessage msg

      WebSocketOpen _ -> do
        logMessage "Socket opened, authenticating."
        lift $ do
          authenticate "test"
          requestBuffers
          requestHistory

      -- TODO: handle these.
      WebSocketClose _ ->
        logMessage "Socket closed."

      WebSocketError _ ->
        logMessage "Socket error."

