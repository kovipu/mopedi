module Mopedi.Store where

import Prelude

type Store =
  { baseUrl :: String
  }

data Action = Nop

reduce :: Store -> Action -> Store
reduce store = case _ of
  Nop -> store
