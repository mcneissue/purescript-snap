module Examples.CatTron.Main where

import Prelude

import Effect (Effect)
import Snap.Machine.FeedbackLoop as FeedbackLoop
import Examples.CatTron.State as State
import Examples.CatTron.UI as UI

main :: Effect Unit
main = FeedbackLoop.simpleMain "container" State.gifLoader UI.component
