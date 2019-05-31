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
import Snap.Component (contraHoistVoid, refocusAll, squash)
import Snap.React (reactTarget, refSnapper)
import Snap.React.Component (counter, input, text, divved)
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
  let s0 = { counter1: 0, counter2: 42, input: "Hello, World" }
  let component = contraHoistVoid launchAff_
                $  (squash <<< refocusAll) { counter1: counter }
                <> (squash <<< refocusAll) { counter2: counter }
                <> (squash <<< refocusAll) { input }
                <> divved text
  e <- element
  ref <- liftEffect $ Ref.new s0
  launchAff_ $ do
    av  <- AVar.empty
    let snapper = refSnapper ref av
    let target = reactTarget e av
    snap snapper component target
