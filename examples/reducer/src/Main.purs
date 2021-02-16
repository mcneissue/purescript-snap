module Examples.Reducer.Main where

import Prelude

import Effect (Effect)
import Examples.Reducer.State as State
import Examples.Reducer.UI as UI
import Snap.Mealy as Mealy

main :: Effect Unit
main = do
  Mealy.simpleMain "container" State.machine UI.component State.initialState State.initialInputs
