module Examples.Reducer.Main where

import Prelude

import Effect (Effect)
import Examples.Reducer.State as State
import Examples.Reducer.UI as UI
import Snap.Machine.FeedbackLoop as FeedbackLoop

main :: Effect Unit
main = FeedbackLoop.simpleMain "container" State.machine UI.component State.initialState
