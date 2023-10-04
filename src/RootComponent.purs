module Mopedi.RootComponent where

import Prelude

import Mopedi.AppM (class LogMessages, logMessage, WebSocketEvent(..), class WeeChat, initConnection, authenticate, requestBuffers, requestHistory)
import Mopedi.Store (ConnectionState(..))

import Control.Monad.Trans.Class (lift)
import Data.Array ((:))
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Tuple.Nested ((/\))
import Data.String (null)
import DOM.HTML.Indexed.InputType (InputType(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { address :: String
  , password :: String
  , connection :: ConnectionState
  , messages :: Array ArrayBuffer
  }

initialState :: State
initialState =
  { address: ""
  , password: ""
  , connection: Disconnected
  , messages: []
  }

data Action
  = AddressChange String
  | PasswordChange String
  | Initialize
  | ReceiveEvent WebSocketEvent

component
  :: forall i q o m
   . LogMessages m
  => WeeChat m
  => H.Component q i o m
component = do
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render { address, password, connection } =
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
        , HE.onClick \_ -> Initialize
        ]
        [ HH.text "Connect" ]
    , HH.p
        []
        [ HH.text
            $ case connection of
                Disconnected -> "Disconnected"
                Connecting -> "Connecting..."
                Connected _ -> "Connected"
                Error -> "Error!?"
        ]
    ]

handleAction
  :: forall o m
   . LogMessages m
  => WeeChat m
  => Action
  -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  AddressChange address ->
    H.modify_ $ _ { address = address }

  PasswordChange password ->
    H.modify_ $ _ { password = password }

  Initialize -> do
    H.modify_ $ _ { connection = Connecting }

    -- Initialize WebSocket connection
    { address } <- H.get
    socket /\ emitter <- lift $ initConnection address
    H.modify_ $ _ { connection = Connected socket }

    -- Send WebSocket events as actions.  
    void $ H.subscribe (ReceiveEvent <$> emitter)

  ReceiveEvent event ->
    case event of
      WebSocketMessage msg ->
        H.modify_ \st -> st { messages = msg : st.messages }

      WebSocketOpen _ -> do
        logMessage "Socket opened, authenticating."
        { password, connection } <- H.get
        lift case connection of
          Connected socket -> do
            authenticate socket password
            requestBuffers socket
            requestHistory socket
          _ -> logMessage "Initialization failed, socket was closed."

      WebSocketClose _ -> do
        H.modify_ $ _ { connection = Disconnected }
        logMessage "Socket closed."

      WebSocketError _ -> do
        H.modify_ $ _ { connection = Error }
        logMessage "Socket error."

