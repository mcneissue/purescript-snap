module Snap.Mealy where

import Prelude

import Control.K (K)
import Control.K as K

import Data.Either (either, Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (traverse_)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens, over)
import Data.Lens.Record (prop)
import Data.Maybe (maybe)
import Data.Profunctor (lcmap)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Effect.Ref as Ref
import React.Basic (JSX)
import React.Basic.DOM as React
import Snap.Component.SYTC (Cmp)
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document, prompt)

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

type Step s i v e = { vertex :: v, transition :: i -> Transition s e }

_transition :: forall a b r. Lens { transition :: a | r } { transition :: b | r } a b
_transition = prop (SProxy :: SProxy "transition")

_vertex :: forall a b r. Lens { vertex :: a | r } { vertex :: b | r } a b
_vertex = prop (SProxy :: SProxy "vertex")

mkTransition :: forall s i e.
  (i -> Transition s e) ->
  Step s i Unit e
mkTransition f = { vertex: unit, transition: f }

-- }}}

-- {{{ Machines

-- A coalgebra (wow, such math)
type Machine s i v e = s -> Step s i v e

-- Greatest fixpoint of coalgebra
newtype Behavior i v e = Behavior (Step (Behavior i v e) i v e)

newtype EBehavior i v e = EBehavior (Step (Effect (EBehavior i v e)) i v e)

-- Effectful machines
type EMachine s i v = Machine s i v (ECont i)

type MMachine m s i v = Machine s i v (m i)

mapI :: ∀ s i i' v e. (i' -> i) -> Machine s i v e -> Machine s i' v e
mapI = map <<< over (_transition <<< lcmap)

mapV :: ∀ s i v v' e. (v -> v') -> Machine s i v e -> Machine s i v' e
mapV = map <<< over _vertex

mapE :: ∀ s i v e e'. (e -> e') -> Machine s i v e -> Machine s i v e'
mapE = map <<< over (_transition <<< map <<< map)

reportState :: ∀ s i v e.
  Machine s i v e -> Machine s i (s /\ v) e
reportState m s = mapV (s /\ _) m s

applyComponent :: forall m s i v e.
  Cmp m v s i ->
  Machine s i Unit e ->
  Machine s i (K (m Unit) v i) e
applyComponent cmp = reportState >>> mapV (fst >>> flip cmp)

-- What to call this??
runVDomMachine :: forall m s i v.
  MonadEffect m =>
  Machine s i (K (m Unit) v i) (MCont m i) ->
  s -> Array i -> MCont m v
runVDomMachine machine init prime render = do
  ref <- liftEffect $ Ref.new init
  let { vertex } = machine init
  render $ vertex $ handleUpdate ref
  traverse_ (handleUpdate ref) prime
  pure unit
  where
  handleUpdate ref i = do
    s <- liftEffect $ Ref.read ref
    let { transition } = machine s
    case transition i of
      No -> liftEffect $ throw "Invalid transition"
      Yes s' effect -> do
        liftEffect $ Ref.write s' ref
        let { vertex } = machine s'
        render $ vertex $ handleUpdate ref
        effect $ handleUpdate ref

runCmpWithMachine :: forall m s i v.
  MonadEffect m =>
  Cmp m v s i ->
  Machine s i Unit (MCont m i) ->
  s -> Array i -> MCont m v
runCmpWithMachine cmp = runVDomMachine <<< applyComponent cmp

splice :: forall s1 s2 i1 i2 v1 v2 e1 e2.
  Machine s1 i1 v1 e1 -> Machine s2 i2 v2 e2 -> Machine (s1 /\ s2) (i1 \/ i2) (v1 /\ v2) (e1 \/ e2)
splice m1 m2 (s1 /\ s2) = case m1 s1 /\ m2 s2 of
  step1 /\ step2 ->
    { vertex: step1.vertex /\ step2.vertex
    , transition: either
        (foldTransition (\s1' e1 -> Yes (s1' /\ s2 ) $ Left  e1) No <<< step1.transition)
        (foldTransition (\s2' e2 -> Yes (s1  /\ s2') $ Right e2) No <<< step2.transition)
   }

par :: ∀ v1 v2.
  ECont v1 /\ ECont v2 -> ECont (v1 \/ v2)
par (k1 /\ k2) cb = k1 (cb <<< Left) *> k2 (cb <<< Right)

esplice :: forall s1 s2 i1 i2 v1 v2.
  EMachine s1 i1 v1 -> EMachine s2 i2 v2 -> EMachine (s1 /\ s2) (i1 \/ i2) (v1 /\ v2)
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
  EMachine (FetchState e r) (FetchUpdate e r) Unit
fetchMachine fetch = case _ of
  Idle -> mkTransition $ case _ of
    Load -> loading
    _ -> No
  Loading -> mkTransition $ case _ of
    Succeed url -> Yes (Success url) K.empty
    Fail error -> Yes (Failure error) K.empty
    _ -> No
  Success url -> mkTransition $ case _ of
    Load -> loading
    _ -> No
  Failure error -> mkTransition $ case _ of
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

simpleMain :: ∀ s i. String -> EMachine s i Unit -> Cmp Effect JSX s i -> s -> Array i -> Effect Unit
simpleMain id machine cmp s i = do
  elem <- element id
  runReact elem $ runCmpWithMachine cmp machine s i

scratch :: Effect Unit
scratch = do
  let
    fm d msg = fetchMachine $ \cb -> launchAff_ (delay (Milliseconds d) *> liftEffect (cb $ (Right msg :: Either Void String)))
    m = reportState $ fm 5000.0 "machine 1" `esplice` fm 1000.0 "machine 2"
  ref <- Ref.new $  Idle /\ Idle
  handleUpdate ref m $ Left Load
  handleUpdate ref m $ Right Load

  where
  handleUpdate ref m u = do
    log $ "Transition occurred: " <> show u
    s <- Ref.read ref

    log $ "Current state: " <> show s

    let step = m s
    case step.transition u of
      No -> log "Produced NO"
      Yes s' effect -> do
        w <- window
        _ <- prompt "Valid transition occured. Press OK to continue" w

        log $ "Writing new state: " <> show s'
        Ref.write s' ref

        log $ "Running transition effects from previous state"
        effect $ handleUpdate ref m
