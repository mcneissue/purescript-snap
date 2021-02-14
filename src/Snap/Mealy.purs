module Snap.Mealy where

import Prelude

import Data.Either (either, Either(..))
import Data.Either.Nested (type (\/))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Bifunctor (lmap)
import Control.Apply (lift2)
import Data.Maybe (maybe)
import Effect.Exception (throw)

import Control.Monad.Cont (ContT(..), runContT)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (error)
import Snap.Component.SYTC (Cmp)
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)
import React.Basic (JSX)
import React.Basic.DOM as React
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

-- {{{ Continutation monad stuff

type ECont = ContT Unit Effect

par :: forall r m a b. Apply m => Semigroup r => ContT r m a /\ ContT r m b -> ContT r m (a \/ b)
par (a /\ b) = ContT \cb -> lift2 append (runContT (Left <$> a) cb) (runContT (Right <$> b) cb)

emptyCont :: ∀ x f a. Applicative f => Monoid x => ContT x f a
emptyCont = ContT \_ -> pure mempty

-- }}}

-- {{{ Machine step

data Transition s e = Yes s e | No

instance functorTransition :: Functor (Transition s) where
  map f No = No
  map f (Yes s e) = Yes s $ f e

type Step s i v e = v /\ (i -> Transition s e)

-- mapStep :: ∀ i v e a b.
--   (a -> b) -> Step s i v e -> Step s i v e
-- mapStep = ?1

-- A coalgebra (wow, such math)
type Machine s i v e = s -> Step s i v e

-- Greatest fixpoint of coalgebra
newtype Behavior i v e = Behavior (Step (Behavior i v e) i v e)

-- Effectful machines
type EMachine s i = Machine s i (ECont i) (ECont i)

type MMachine m s i = Machine s i (m i) (m i)

type EBehavior i = Behavior i (ECont i) (ECont i)

mapV :: forall v v' s i e. (v -> v') -> Machine s i v e -> Machine s i v' e
mapV = map <<< lmap

mapE :: forall v s i e e'. (e -> e') -> Machine s i v e -> Machine s i v e'
mapE = map <<< map <<< map <<< map

unfoldMachine :: ∀ s i v e.
  Machine s i v e -> s -> Behavior i v e
unfoldMachine machine = go
  where
  go s = Behavior $ case machine s of
    v /\ f -> v /\ \i -> f i # case _ of
      No -> No
      Yes s' e -> Yes (go s') e

splice :: forall s1 s2 i1 i2 v1 v2 e1 e2.
  Machine s1 i1 v1 e1 -> Machine s2 i2 v2 e2 -> Machine (s1 /\ s2) (i1 \/ i2) (v1 /\ v2) (e1 \/ e2)
splice m1 m2 (s1 /\ s2) = case m1 s1 /\ m2 s2 of
  (v1 /\ t1) /\ (v2 /\ t2) -> (v1 /\ v2) /\ either
                                (\i1 ->
                                  case t1 i1 of
                                    No -> No
                                    Yes s1' e1 -> Yes (s1' /\ s2) (Left e1)
                                )
                                (\i2 ->
                                  case t2 i2 of
                                    No -> No
                                    Yes s2' e2 -> Yes (s1 /\ s2') (Right e2)
                                )

esplice :: forall s1 s2 i1 i2.
  EMachine s1 i1 -> EMachine s2 i2 -> EMachine (s1 /\ s2) (i1 \/ i2)
esplice m1 m2 = mapE (either (map Left) (map Right)) $ mapV par $ splice m1 m2

encapsulate :: ∀ s u v.
  EMachine s u -> Cmp Effect v s u -> s -> ECont v
encapsulate m render s = ContT $ \cb -> do
  let
    handleNewState current = do
      -- Get the background task for the current state
      let background /\ _ = m current
      -- Render the new UI
      cb $ render (handleUpdate current) current
      -- Run the background task
      -- Order is important here, the background task must be run after the component is rendered
      runContT background (handleUpdate current)

    handleUpdate s' u = do
      -- Run the machine to get the transition
      let _ /\ transition = m s'
      -- Run the transition to get the next state and the side effect
      case transition u of
        No -> error "Bad transition was applied to the machine, to you sir we say: 🖕"
        Yes nextState effect -> do
          -- Render the new UI
          handleNewState nextState
          -- Run the side effect
          runContT effect (handleUpdate nextState)

  handleNewState s

-- }}}


-- {{{ Fetch machine

data FetchState e r = Idle | Loading | Success r | Failure e
data FetchUpdate e r = Load | Cancel | Succeed r | Fail e

derive instance genericFetchState :: Generic (FetchState e r) _

instance showFetchState :: (Show e, Show r) => Show (FetchState e r) where
  show = genericShow

fetch :: forall e r.
  ECont (e \/ r) ->
  EMachine (FetchState e r) (FetchUpdate e r)
fetch f = case _ of

  Idle -> pure Load /\ case _ of
    Load -> Yes Loading $ f <#> either Fail Succeed
    _ -> No

  Loading -> emptyCont /\ case _ of
    Succeed url -> Yes (Success url) emptyCont
    Fail error -> Yes (Failure error) emptyCont
    _ -> No

  Success url -> emptyCont /\ case _ of
    Load -> Yes Idle emptyCont
    _ -> No

  Failure error -> emptyCont /\ case _ of
    Load -> Yes Idle emptyCont
    _ -> No

-- }}}

runReact :: forall m. MonadEffect m => Element -> ContT Unit m JSX -> m Unit
runReact e (ContT f) = f \v -> liftEffect $ React.render v e

element :: String -> Effect Element
element id = do
  mc <- window >>= document <#> toNonElementParentNode >>= getElementById id
  maybe (throw "Couldn't find root element") pure mc

simpleMain :: forall s u. String -> EMachine s u -> Cmp Effect JSX s u -> s -> Effect Unit
simpleMain id machine cmp s = do
  elem <- element id
  runReact elem $ encapsulate machine cmp s
