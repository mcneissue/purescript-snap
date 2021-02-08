module Examples.CatTron.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Examples.CatTron.State as State
import Examples.CatTron.UI as UI
import Snap.Component.SYTC (contraHoist)
import Snap.Machine.FeedbackLoop as FeedbackLoop

main :: Effect Unit
main = launchAff_ $ do
  machine <- State.machine
  FeedbackLoop.simpleMain "container" machine (contraHoist launchAff_ UI.component) State.initialState
