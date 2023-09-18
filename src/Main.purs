module Main where

import Prelude

import Mopedi.App.Button as Button
import Mopedi.AppM (runAppM)
import Mopedi.Store (Store)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody

  let
    baseUrl :: String
    baseUrl = ""

    initialStore :: Store
    initialStore = { baseUrl }

  rootComponent <- runAppM initialStore Button.component

  runUI rootComponent unit body

