module Examples.Routing.Main where

import Prelude

import Data.Maybe (maybe, Maybe(..))
import Effect (Effect)
import Effect.Aff (error, launchAff_)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Exception (throwException)
import Effect.Ref as Ref
import Examples.Routing.Router as Router
import Examples.Routing.State (Action(..), initialState, reducer)
import Examples.Routing.UI (app)
import Snap (encapsulate, snap)
import Snap.Component.SYTC (map) as C
import Snap.React (reactTargetM, refSnapper)
import Snap.Snapper (Snapper(..), reduced)
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

-- Finding the DOM element we're going to render everything onto
element :: Effect Element
element = do
  mc <- window >>= document <#> toNonElementParentNode >>= getElementById "container"
  maybe (throwException (error "Couldn't find root element")) pure mc

main :: Effect Unit
main = do
  -- Find the DOM element and create an Ref to hold the application state
  e <- element
  ref <- liftEffect $ Ref.new initialState
  launchAff_ $ do
    av  <- AVar.empty
    -- Create the state manager and target from the resources above
    let snapper = reduced reducer $ refSnapper ref av
    let (Snapper snapper') = snapper
    let cmp = C.map join $ encapsulate snapper app
    let target = reactTargetM e av
    -- Snap everything together
    _ <- Router.mkRouter \mr r -> when (mr /= Just r) $ snapper'.put $ Navigate r
    snap cmp target
