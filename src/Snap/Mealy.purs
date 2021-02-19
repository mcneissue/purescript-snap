module Snap.Mealy where

import Prelude

import Control.K (K)
import Control.K as K
import Data.Either (either, Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (traverse_)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (maybe)
import Data.Profunctor (lcmap)
import Data.Tuple.Nested (type (/\), (/\))
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

-- {{{ Continuation monad stuff

type MCont m v = K (m Unit) (m Unit) v
type ECont v = MCont Effect v

-- }}}

-- {{{ Machine step

data Transition s e = Yes s e | No

instance functorTransition :: Functor (Transition s) where
  map f No = No
  map f (Yes s e) = Yes s $ f e

foldTransition :: forall s e r. (s -> e -> r) -> r -> Transition s e -> r
foldTransition yes no = case _ of
  No -> no
  Yes s e -> yes s e

type Step s i e = i -> Transition s e

-- }}}

-- {{{ Machines

-- A coalgebra (wow, such math)
type Machine s i e = s -> Step s i e

-- Greatest fixpoint of coalgebra
newtype Behavior i e = Behavior (Step (Behavior i e) i e)

newtype EBehavior i e = EBehavior (Step (Effect (EBehavior i e)) i e)

-- Effectful machines
type EMachine s i = Machine s i (ECont i)

type MMachine m s i = Machine s i (m i)

mapI :: ∀ s i i' e. (i' -> i) -> Machine s i e -> Machine s i' e
mapI = map <<< lcmap

mapE :: ∀ s i e e'. (e -> e') -> Machine s i e -> Machine s i e'
mapE = map <<< map <<< map

-- What to call this??
runVDomMachine :: forall m s i v.
  MonadEffect m =>
  Cmp m v s i ->
  Machine s i (MCont m i) ->
  s -> Array i -> MCont m v
runVDomMachine cmp machine init prime render = do
  ref <- liftEffect $ Ref.new init
  let vdom = cmp (handleUpdate ref) init
  render $ vdom
  traverse_ (handleUpdate ref) prime
  pure unit
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

splice :: forall s1 s2 i1 i2 e1 e2.
  Machine s1 i1 e1 -> Machine s2 i2 e2 -> Machine (s1 /\ s2) (i1 \/ i2) (e1 \/ e2)
splice m1 m2 (s1 /\ s2) = case m1 s1 /\ m2 s2 of
  step1 /\ step2 -> either
    (foldTransition (\s1' e1 -> Yes (s1' /\ s2 ) $ Left  e1) No <<< step1)
    (foldTransition (\s2' e2 -> Yes (s1  /\ s2') $ Right e2) No <<< step2)

par :: ∀ v1 v2.
  ECont v1 /\ ECont v2 -> ECont (v1 \/ v2)
par (k1 /\ k2) cb = k1 (cb <<< Left) *> k2 (cb <<< Right)

esplice :: forall s1 s2 i1 i2.
  EMachine s1 i1 -> EMachine s2 i2 -> EMachine (s1 /\ s2) (i1 \/ i2)
esplice m1 m2 = mapE K.diverge $ splice m1 m2

-- {{{ Fetch machine

data FetchState e r = Idle | Loading | Success r | Failure e

derive instance genericFetchState :: Generic (FetchState e r) _

instance showFetchState :: (Show e, Show r) => Show (FetchState e r) where
  show = genericShow

data FetchUpdate e r = Load | Cancel | Succeed r | Fail e

derive instance genericFetchUpdate :: Generic (FetchUpdate e r) _

instance showFetchUpdate :: (Show e, Show r) => Show (FetchUpdate e r) where
  show = genericShow

fetchMachine :: forall e r.
  ECont (e \/ r) ->
  EMachine (FetchState e r) (FetchUpdate e r)
fetchMachine fetch = case _ of
  Idle -> case _ of
    Load -> loading
    _ -> No
  Loading -> case _ of
    Succeed url -> Yes (Success url) K.empty
    Fail error -> Yes (Failure error) K.empty
    _ -> No
  Success url -> case _ of
    Load -> loading
    _ -> No
  Failure error -> case _ of
    Load -> loading
    _ -> No
  where
  loading = Yes Loading $ (fetch K.<#> either Fail Succeed)

-- }}}

runReact :: ∀ m. MonadEffect m => Element -> MCont m JSX -> m Unit
runReact e f = f \v -> liftEffect $ React.render v e

element :: String -> Effect Element
element id = do
  mc <- window >>= document <#> toNonElementParentNode >>= getElementById id
  maybe (throw "Couldn't find root element") pure mc

simpleMain :: ∀ s i. String -> EMachine s i -> Cmp Effect JSX s i -> s -> Array i -> Effect Unit
simpleMain id machine cmp s i = do
  elem <- element id
  runReact elem $ runVDomMachine cmp machine s i

