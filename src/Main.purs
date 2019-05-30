module Main where

import Prelude

import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Aff (error, launchAff_)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Exception (throwException)
import Effect.Ref as Ref
import Snap (snap)
import Snap.Component (contraHoistVoid)
import Snap.React (reactTarget, refSnapper)
import Snap.React.Component (twoCountersAndAnInput)
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

element :: Effect Element
element = do
  mc <- window >>= document <#> toNonElementParentNode >>= getElementById "container"
  maybe (throwException (error "Couldn't find root element")) pure mc

main :: Effect Unit
main = do
  let s0 = { counter1: 0, counter2: 42, input: "Hello" }
  e <- element
  ref <- liftEffect $ Ref.new s0
  launchAff_ $ do
    av  <- AVar.empty
    let snapper = refSnapper ref av
    let target = reactTarget e av
    snap snapper (contraHoistVoid launchAff_ twoCountersAndAnInput) target
