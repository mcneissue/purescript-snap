module Snap.Machine.FeedbackLoop where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Cont (ContT(..), runContT)
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Maybe (maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, ParAff, error)
import Effect.Aff as Aff
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throwException)
import Effect.Ref as Ref
import React.Basic (JSX)
import React.Basic.DOM as React
import Snap.Component.SYTC (Cmp)
import Snap.Machine as Machine
import Snap.Machine.Type (Machine)
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

type FeedbackLoop r m s u = Machine s u (ContT r m u)

splice :: forall r m s1 u1 s2 u2. Apply m => Semigroup r => FeedbackLoop r m s1 u1 -> FeedbackLoop r m s2 u2 -> FeedbackLoop r m (s1 /\ s2) (u1 \/ u2)
splice f1 f2 (s1 /\ s2) = case f1 s1 /\ f2 s2 of
  (task1 /\ trans1) /\ (task2 /\ trans2) -> par (task1 /\ task2) /\ either (\u1 -> trans1 u1 /\ s2) (\u2 -> s1 /\ trans2 u2)

par :: forall r m a b. Apply m => Semigroup r => ContT r m a /\ ContT r m b -> ContT r m (a \/ b)
par (a /\ b) = ContT \cb -> lift2 append (runContT (Left <$> a) cb) (runContT (Right <$> b) cb)

hoist :: forall m n r s u. (m ~> n) -> (n ~> m) -> FeedbackLoop r m s u -> FeedbackLoop r n s u
hoist n1 n2 f = Machine.mapO (hoistCont n1 n2) f

hoistCont :: forall m n r. (m ~> n) -> (n ~> m) -> ContT r m ~> ContT r n
hoistCont n1 n2 (ContT m) = ContT \cb -> n1 (m $ n2 <<< cb)

encapsulatePure :: forall m v s u.
  Monad m =>
  FeedbackLoop Unit m s u ->
  Cmp m v s u ->
  s -> ContT Unit m v
encapsulatePure machine cmp = loop
  where
  loop s = case machine s of
    task /\ transition -> ContT $ \cb ->
      let recurse u = pure unit >>= \_ -> runContT (loop $ transition u) cb
      in cb (cmp recurse s) *> runContT task recurse

encapsulate :: forall m v s u.
  MonadEffect m =>
  FeedbackLoop Unit m s u ->
  Cmp m v s u ->
  s -> ContT Unit m v
encapsulate machine cmp init = do
  ref <- liftEffect $ Ref.new init
  loop ref
  where
  loop ref =
    ContT $ \cb -> do
      s <- liftEffect $ Ref.read ref
      let 
        task /\ _ = machine s
        recurse u = do
          s' <- liftEffect $ Ref.read ref
          let _ /\ transition = machine s'
          runContT (liftEffect (Ref.write (transition u) ref) *> loop ref) cb
      cb (cmp recurse s) *> runContT task recurse

runReact :: forall m. MonadEffect m => Element -> ContT Unit m JSX -> m Unit
runReact e (ContT f) = f \v -> liftEffect $ React.render v e

element :: String -> Effect Element
element id = do
  mc <- window >>= document <#> toNonElementParentNode >>= getElementById id
  maybe (throwException (error "Couldn't find root element")) pure mc

simpleMain :: forall m s u. MonadEffect m => String -> FeedbackLoop Unit m s u -> Cmp m JSX s u -> s -> m Unit
simpleMain id machine cmp s = do
  elem <- liftEffect $ element id
  runReact elem $ encapsulate machine cmp s

emptyCont :: ∀ x f a. Applicative f => Monoid x => ContT x f a
emptyCont = ContT \_ -> pure mempty