module Main where

import Prelude

import Data.Maybe (maybe)
import Data.Profunctor (lcmap)
import Effect (Effect)
import Effect.Aff (error, launchAff_)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Exception (throwException)
import Effect.Ref as Ref
import Snap (snap)
import Snap.Component (contraHoist, focus)
import Snap.React (reactTarget, refSnapper)
import Snap.React.Component (counter, input, text, divved, list) as S
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
  let component =  contraHoist launchAff_
                $  focus { counter1: S.counter }
                <> focus { counter2: S.counter }
                <> focus { input: S.input }
                <> focus { input: S.divved $ lcmap show $ S.text }
  let state = [s0, s0, s0]
  let app = (S.divved <<< S.list) component <> (S.divved <<< lcmap show) S.text
  e <- element
  ref <- liftEffect $ Ref.new state
  launchAff_ $ do
    av  <- AVar.empty
    let snapper = refSnapper ref av
    let target = reactTarget e av
    snap snapper app target
