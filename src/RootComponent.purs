module Mopedi.RootComponent where

import Mopedi.AppM (class LogMessages, logMessage)
import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = { count :: Int }

data Query a = ReceiveMessage String a

newtype Message = Message String

data Action = Increment

component :: forall i m. LogMessages m => H.Component Query i Message m
component =
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
    ]

handleAction :: forall m. LogMessages m => Action → H.HalogenM State Action () Message m Unit
handleAction = case _ of
  Increment -> do
    _ <- logMessage "increment"
    let password = "test"
    H.raise $ Message $ "init password=" <> password <> ",compression=off\n"
    H.raise $ Message $ "(hdata_buffers) hdata buffer:gui_buffers(*) number,full_name,short_name\n"
    H.modify_ \st -> st { count = st.count + 1 }
