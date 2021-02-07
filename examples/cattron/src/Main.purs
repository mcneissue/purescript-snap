module Examples.CatTron.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Examples.CatTron.State as State
import Examples.CatTron.UI as UI
import Snap.Machine.FeedbackLoop as FeedbackLoop

main :: Effect Unit
main = launchAff_ $ do
  avar <- AVar.empty
  liftEffect $ FeedbackLoop.simpleMain "container" (State.gifLoader avar) UI.component FeedbackLoop.Loading
