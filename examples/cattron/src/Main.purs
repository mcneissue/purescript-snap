module Examples.CatTron.Main where

import Prelude

import Effect (Effect)
import Examples.CatTron.State as State
import Examples.CatTron.UI as UI
import Snap.Machine.FeedbackLoop as FeedbackLoop

main :: Effect Unit
main = FeedbackLoop.simpleMain "container" State.gifLoader UI.component FeedbackLoop.Loading
