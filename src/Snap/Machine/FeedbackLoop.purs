module Snap.Machine.FeedbackLoop where

import Prelude

import Control.Monad.Cont (ContT(..), runContT)
import Data.Maybe (maybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throwException)
import React.Basic (JSX)
import React.Basic.DOM as React
import Snap.Component.SYTC (Cmp, contraHoist)
import Snap.Machine.Type (Machine)
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

type FeedbackLoop r m s u = Machine s u (ContT r m u)

encapsulate :: forall m v s u.
  MonadAff m =>
  FeedbackLoop Unit m s u ->
  Cmp m v s u ->
  s -> ContT Unit m v
encapsulate machine cmp = loop
  where
  loop s = case machine s of
    task /\ transition -> ContT $ \cb ->
      let recurse u = pure unit >>= \_ -> runContT (loop $ transition u) cb
      in cb (cmp recurse s) *> runContT task recurse

runReact :: forall m. MonadEffect m => Element -> ContT Unit m JSX -> m Unit
runReact e (ContT f) = f \v -> liftEffect $ React.render v e

element :: String -> Effect Element
element id = do
  mc <- window >>= document <#> toNonElementParentNode >>= getElementById id
  maybe (throwException (error "Couldn't find root element")) pure mc

simpleMain :: forall s u. String -> FeedbackLoop Unit Aff s u -> Cmp Effect JSX s u -> s -> Effect Unit
simpleMain id machine cmp s = do
  elem <- element id
  launchAff_ $ runReact elem $ encapsulate machine (contraHoist launchAff_ cmp) s
