module Examples.TransactionalForm.Main where

import Prelude

import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Aff (error, launchAff_)
import Effect.Aff.AVar as AVar
import Effect.Exception (throwException)
import Examples.TransactionalForm.State (snapper)
import Examples.TransactionalForm.UI (app)
import Snap (encapsulate, snap)
import Snap.React (reactTargetM)
import Snap.Component.SYTC (contraHoist_)
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
  launchAff_ $ do
    av  <- AVar.empty
    -- Create the state manager and target from the resources above
    snapper <- snapper av
    let target = reactTargetM e av
    let cmp = encapsulate snapper $ contraHoist_ launchAff_ $ app
    -- Snap everything together
    snap cmp target
