module Mopedi.App.Button where

import Mopedi.AppM (class LogMessages, logMessage)
import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = { count :: Int }

data Action = Increment

component :: forall q i o m. LogMessages m => H.Component q i o m
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

handleAction :: forall cs o m. LogMessages m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Increment -> do
    _ <- logMessage "increment"
    H.modify_ \st -> st { count = st.count + 1 }
