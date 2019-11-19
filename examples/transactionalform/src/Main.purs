module Examples.TransactionalForm.Main where

import Prelude

import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Aff (error, launchAff_)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Exception (throwException)
import Effect.Ref as Ref
import Snap (snap)
import Snap.React (reactTarget, refSnapper)
import Snap.SYTC.Component (contraHoist)
import Examples.TransactionalForm.State (initialState)
import Examples.TransactionalForm.UI (app)
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
    let snapper = refSnapper ref av
    let target = reactTarget e av
    -- Snap everything together
    snap snapper (contraHoist launchAff_ $ app) target
