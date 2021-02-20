module Snap where

import Prelude

import Control.K as K
import Data.Foldable (traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import React.Basic (JSX)
import React.Basic.DOM as React
import Snap.Component.SYTC (Cmp)
import Snap.Machine (EMachine)
import Snap.Machine.SYTC (Machine)
import Snap.Machine.SYTC as Machine
import Snap.Machine.SYTC (Behavior)
import Snap.Machine.Step (Transition(..))
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

type Snapper m u s = { get :: m s, set :: u -> m Unit, error :: m Unit }
type Snapper' m s = Snapper m s s

-- What to call this??
runVDomMachine :: forall m s i v.
  Monad m =>
  Snapper' m (Behavior i (s /\ K.MK m i)) ->
  Cmp m v s i ->
  s -> K.K (m Unit) (m (i -> m Unit)) v
runVDomMachine interp cmp init render = do
  -- initial render
  render $ cmp handleUpdate init
  pure handleUpdate
  where
  handleUpdate i = do
    Machine.Behavior m <- interp.get
    case m i of
      No -> interp.error
      Yes rest (s' /\ effect) -> do
        interp.set rest
        render $ cmp handleUpdate s'
        effect handleUpdate

effectInterpreter :: forall m x. MonadEffect m => x -> m { get :: m x, set :: x -> m Unit, error :: m Unit }
effectInterpreter init = do
  ref <- liftEffect $ Ref.new init
  pure
    { get: liftEffect $ Ref.read ref
    , set: liftEffect <<< flip Ref.write ref
    , error: liftEffect $ throw "Invalid transition."
    }

runReact :: ∀ m. MonadEffect m => Element -> JSX -> m Unit
runReact e v = liftEffect $ React.render v e

element :: String -> Effect Element
element id = do
  mc <- window >>= document <#> toNonElementParentNode >>= getElementById id
  maybe (throw "Couldn't find root element") pure mc

simpleMain :: ∀ s i. String -> EMachine s i -> Cmp Effect JSX s i -> s -> Array i -> Effect Unit
simpleMain id machine cmp s i = do
  elem <- element id
  let b = Machine.unfold (Machine.reportState machine) s
  interpreter <- effectInterpreter b
  handleUpdate <- runVDomMachine interpreter cmp s (runReact elem)
  traverse_ handleUpdate i
