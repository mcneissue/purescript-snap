module Snap.Machine.FeedbackLoop where
  
import Prelude

import Control.Monad.Cont (ContT(..), runContT)
import Data.Either (Either, either)
import Data.Maybe (maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, delay, error, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
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

-- example

data State r e = Loading | Success r | Failure e
data Transition r e = Reload | Succeed r | Fail e

loader :: forall m res err.
  Monad m =>
  m (Either err res) ->
  FeedbackLoop Unit m (State res err) (Transition res err)
loader load state = update /\ transition
  where
  update = case state of
    Loading -> ContT (\cb -> load >>= either (cb <<< Fail) (cb <<< Succeed))
    Success r -> emptyCont
    Failure e -> emptyCont
  transition t = case t of
    Reload -> Loading
    Succeed r -> Success r
    Fail e -> Failure e

emptyCont :: forall x f a. Applicative f => Monoid x => ContT x f a
emptyCont = ContT \_ -> pure mempty

encapsulate :: forall m v s u.
  MonadAff m =>
  FeedbackLoop Unit m s u -> 
  Cmp m v s u ->
  s -> ContT Unit m v
encapsulate machine cmp = loop
  where
  loop s = case machine s of
    task /\ transition -> ContT $ \cb ->
      let recurse u = runContT (loop $ transition u) cb
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
