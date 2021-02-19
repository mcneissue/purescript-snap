module Snap where

import Prelude

import Snap.Machine (Machine, EMachine)
import Snap.Machine.Step (Transition(..))
import Control.K as K
import Data.Foldable (traverse_)
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import React.Basic (JSX)
import React.Basic.DOM as React
import Snap.Component.SYTC (Cmp)
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

-- What to call this??
runVDomMachine :: forall m s i v.
  MonadEffect m =>
  Cmp m v s i ->
  Machine s i (K.MK m i) ->
  s -> K.K (m Unit) (m (i -> m Unit)) v
runVDomMachine cmp machine init render = do
  ref <- liftEffect $ Ref.new init
  let vdom = cmp (handleUpdate ref) init
  render vdom
  pure $ handleUpdate ref
  where
  handleUpdate ref i = do
    s <- liftEffect $ Ref.read ref
    let transition = machine s
    case transition i of
      No -> liftEffect $ throw "Invalid transition"
      Yes s' effect -> do
        liftEffect $ Ref.write s' ref
        let vdom = cmp (handleUpdate ref) s'
        render vdom
        effect $ handleUpdate ref

runReact :: ∀ m. MonadEffect m => Element -> JSX -> m Unit
runReact e v = liftEffect $ React.render v e

element :: String -> Effect Element
element id = do
  mc <- window >>= document <#> toNonElementParentNode >>= getElementById id
  maybe (throw "Couldn't find root element") pure mc

simpleMain :: ∀ s i. String -> EMachine s i -> Cmp Effect JSX s i -> s -> Array i -> Effect Unit
simpleMain id machine cmp s i = do
  elem <- element id
  handleUpdate <- runVDomMachine cmp machine s (runReact elem)
  traverse_ handleUpdate i
