module Examples.Routing.Main where

import Prelude

import Data.Maybe (maybe)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.Aff (error, launchAff_)
import Effect.Aff.AVar (put)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Exception (throwException)
import Examples.Routing.State (snapper)
import Examples.Routing.UI (app)
import Routing.Hash (foldHashes)
import Snap (encapsulate, snap)
import Snap.Component.SYTC (map) as C
import Snap.React (reactTargetM)
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

subscribeHash :: AVar Unit -> Effect Unit
subscribeHash av = void $ foldHashes (\_ _ -> launchAff_ $ put unit av) (const $ pure unit)

main :: Effect Unit
main = do
  -- Find the DOM element and create an Ref to hold the application state
  e <- element
  launchAff_ $ do
    av  <- AVar.empty
    -- Create the state manager and target from the resources above
    s <- snapper av
    let cmp = C.map join $ encapsulate s app
    let target = reactTargetM e av
    liftEffect $ subscribeHash av
    -- Snap everything together
    snap cmp target
