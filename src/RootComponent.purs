module Mopedi.RootComponent where

import Prelude

import Mopedi.AppM (class LogMessages, logMessage, WebSocketEvent(..), class WeeChat, initConnection, authenticate, requestBuffers, requestHistory)
import Mopedi.Store (Action(..), Store) as Store

import Data.String (null)
import Control.Monad.Trans.Class (lift)
import DOM.HTML.Indexed.InputType (InputType(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore, updateStore)

type State = { address :: String, password :: String }

data Action
  = AddressChange String
  | PasswordChange String
  | Initialize
  | ReceiveEvent WebSocketEvent

component
  :: forall i q o m
   . LogMessages m
  => WeeChat m
  => MonadStore Store.Action Store.Store m
  => H.Component q i o m
component = do
  H.mkComponent
    { initialState: \_ -> { address: "", password: "" }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render { address, password } =
  HH.div_
    [ HH.input
        [ HP.value address
        , HE.onValueInput \addr -> AddressChange addr
        ]
    , HH.input
        [ HP.value password
        , HE.onValueInput \pw -> PasswordChange pw
        , HP.type_ InputPassword
        ]
    , HH.button
        [ HP.disabled $ (null address) || (null password)
        , HE.onClick \_ -> Initialize ]
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
  AddressChange address ->
    H.modify_ \st -> st { address = address }

  PasswordChange password ->
    H.modify_ \st -> st { password = password }

  Initialize -> do
    { address } <- H.get
    -- Initializer WebSocket connection
    emitter <- lift $ initConnection address
    -- Send WebSocket events to the store.  
    void $ H.subscribe (ReceiveEvent <$> emitter)

  ReceiveEvent event ->
    case event of
      WebSocketMessage msg ->
        updateStore $ Store.NewMessage msg

      WebSocketOpen _ -> do
        logMessage "Socket opened, authenticating."
        { password } <- H.get
        lift $ do
          authenticate password
          requestBuffers
          requestHistory

      -- TODO: handle these.
      WebSocketClose _ ->
        logMessage "Socket closed."

      WebSocketError _ ->
        logMessage "Socket error."

