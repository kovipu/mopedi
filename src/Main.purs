module Main where

import Prelude

import Mopedi.RootComponent (component)
import Mopedi.AppM (runAppM)

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody

    rootComponent <- runAppM unit component

    runUI rootComponent unit body
