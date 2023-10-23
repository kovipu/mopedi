module Mopedi.RootComponent where

import Prelude

import Mopedi.AppM (class LogMessages, logMessage, WebSocketEvent(..), class WeeChat, initConnection, authenticate, requestBuffers, requestHistory, requestSync)
import Mopedi.Store (ConnectionState(..))
import Mopedi.WeeChatParser (parseWeeChatMsg, WeeChatMessage(..), HistoryRow)

import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl, find)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Data.String (null)
import DOM.HTML.Indexed.InputType (InputType(..))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { address :: String
  , password :: String
  , connection :: ConnectionState
  , selectedBuffer :: Maybe String
  , buffers :: Map String BufferState
  }

type BufferState =
  { number :: Int
  , name :: String
  , ppath :: String
  , history :: Array HistoryRow
  }

initialState :: State
initialState =
  { address: "ws://localhost:8001/weechat"
  , password: "test"
  , connection: Disconnected
  , selectedBuffer: Nothing
  , buffers: Map.empty
  }

data Action
  = AddressChange String
  | PasswordChange String
  | Initialize
  | ReceiveEvent WebSocketEvent
  | ChangeBuffer String

component
  :: forall i q o m
   . LogMessages m
  => WeeChat m
  => MonadEffect m
  => H.Component q i o m
component = do
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state@{ connection } =
  case connection of
    Connected _socket ->
      HH.div
        [ HP.class_ $ HH.ClassName "h-screen w-screen flex" ]
        [ bufferList state
        , chatContainer state
        ]
    _ ->
      loginForm state

bufferList :: forall cs m. State -> H.ComponentHTML Action cs m
bufferList { buffers, selectedBuffer } =
  HH.div
    [ HP.class_ $ HH.ClassName "h-full w-56 flex flex-col px-3 py-2" ]
    bufferButtons
  where
  bufferButtons =
    Array.fromFoldable buffers
      # Array.sortBy (\a b -> compare a.number b.number)
      # map bufferButton

  bufferButton { name, ppath } =
    HH.button
      [ HP.class_ $ HH.ClassName $
          "text-left px-3 py-1"
            <>
              if selected then " bg-gray-300 rounded"
              else ""
      , HE.onClick $ const $ ChangeBuffer ppath
      ]
      [ HH.text name ]
    where
    selected = selectedBuffer == Just ppath

chatContainer :: forall cs m. State -> H.ComponentHTML Action cs m
chatContainer { selectedBuffer, buffers } =
  HH.div
    [ HP.class_ $ HH.ClassName "h-full w-full flex flex-col-reverse bg-white w-full overflow-y-auto" ]
    $ Array.reverse messages
  where
  selected =
    (\b -> Map.lookup b buffers) <$> selectedBuffer

  messages =
    case selected of
      Nothing -> [ HH.text "No buffer selected." ]
      Just Nothing -> [ HH.text "Invalid selected buffer!" ]
      Just (Just { history }) ->
        map
          ( \{ message, prefix } ->
              HH.div
                []
                [ HH.p
                    [ HP.class_ $ HH.ClassName "px-3 pt-3 font-bold" ]
                    (renderColoredText prefix)
                , HH.p
                    [ HP.class_ $ HH.ClassName "px-3 py-1" ]
                    $ renderColoredText message
                ]
          )
          history

  renderColoredText message =
    map
      ( \{ content, color } ->
          case color of
            0 -> HH.text content
            n -> HH.em
              [ HP.class_ $ HH.ClassName $ "not-italic " <> getColor n ]
              [ HH.text content ]
      )
      message
    where
    getColor =
      case _ of
        131 -> "text-red-600"
        5 -> "text-emerald-700"
        6 -> "text-gray-400"
        7 -> "text-yellow-600"
        12 -> "text-purple-700"
        13 -> "text-teal-700"
        14 -> "text-rose-700"
        15 -> "text-lime-700" -- own user color
        246 -> "text-gray-400"
        n -> "color-" <> show n

loginForm :: forall cs m. State -> H.ComponentHTML Action cs m
loginForm { address, password, connection } =
  HH.div
    [ HP.class_ $ HH.ClassName "flex flex-col gap-2 w-96" ]
    [ HH.input
        [ HP.class_ $ HH.ClassName "p-2 rounded"
        , HP.value address
        , HE.onValueInput AddressChange
        ]
    , HH.input
        [ HP.class_ $ HH.ClassName "p-2 rounded"
        , HP.value password
        , HE.onValueInput PasswordChange
        , HP.type_ InputPassword
        ]
    , HH.button
        [ HP.class_ $ HH.ClassName "p-2 border-2 border-white rounded font-bold hover:bg-gray-100"
        , HP.disabled $ (null address) || (null password)
        , HE.onClick $ const Initialize
        ]
        [ HH.text "Connect" ]
    , HH.p_
        [ HH.text
            $ case connection of
                Disconnected -> "Disconnected"
                Connecting -> "Connecting..."
                Connected _ -> "Connected"
                Error -> "Highkey cringe af."
        ]
    ]

handleAction
  :: forall o m
   . LogMessages m
  => MonadEffect m
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

  ChangeBuffer ppath ->
    H.modify_ $ _ { selectedBuffer = Just ppath }

  ReceiveEvent event ->
    case event of
      WebSocketMessage msg -> do
        parsedMsg <- liftEffect $ parseWeeChatMsg msg
        case parsedMsg of
          Left error ->
            logMessage $ "Failed to parse WeeChatMessage " <> show error

          Right (Buffers newBuffers) ->
            H.modify_
              ( \st@{ buffers } -> st
                  { buffers = insertBuffers buffers
                  , selectedBuffer = selectedBuffer
                  }
              )
            where
            insertBuffers :: Map String BufferState -> Map String BufferState
            insertBuffers prevState = foldl
              ( \acc { ppath, number, fullName, shortName } ->
                  Map.insertWith
                    (\prev new -> new { history = prev.history })
                    ppath
                    { number
                    , ppath
                    , name: fromMaybe fullName shortName
                    , history: []
                    }
                    acc
              )
              prevState
              newBuffers

            -- Select the first buffer by default
            selectedBuffer :: Maybe String
            selectedBuffer =
              newBuffers
                # find (\{ number } -> number == 1)
                # map _.ppath

          Right (History history) ->
            H.modify_
              ( \st@{ buffers } ->
                  st { buffers = insertHistory buffers }
              )
            where
            insertHistory :: Map String BufferState -> Map String BufferState
            insertHistory bufferState =
              foldl
                ( \acc histRow ->
                    Map.insertWith
                      (\prev new -> prev { history = prev.history <> new.history })
                      histRow.buffer
                      { name: "Unknown buffer"
                      , ppath: histRow.buffer
                      , number: 99
                      , history: [ histRow ]
                      }
                      acc
                )
                bufferState
                history
          Right (NewLine histRow) ->
            H.modify_
              ( \st@{ buffers } ->
                  st { buffers = insertMessage buffers }
              )
            where
            insertMessage :: Map String BufferState -> Map String BufferState
            insertMessage bufferState =
              Map.update
                (\prev@{ history } -> Just $ prev { history = Array.snoc history histRow })
                histRow.buffer
                bufferState

      WebSocketOpen _ -> do
        logMessage "Socket opened, authenticating."
        { password, connection } <- H.get
        lift case connection of
          Connected socket -> do
            authenticate socket password
            requestBuffers socket
            requestHistory socket
            requestSync socket
          _ -> logMessage "Initialization failed, socket was closed."

      WebSocketClose _ -> do
        H.modify_ $ _ { connection = Disconnected }
        logMessage "Socket closed."

      WebSocketError _ -> do
        H.modify_ $ _ { connection = Error }
        logMessage "Socket error."

