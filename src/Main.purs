module Main where

import Prelude

import Mopedi.RootComponent (component)
import Mopedi.AppM (runAppM)
import Mopedi.Store as Store

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody

    rootComponent <- runAppM Store.initialStore component

    runUI rootComponent unit body
