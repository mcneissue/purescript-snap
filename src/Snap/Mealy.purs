module Snap.Mealy where

import Prelude

import Control.K (K)
import Control.K as K
import Data.Bifunctor (lmap)
import Data.Either (either, Either(..))
import Data.Either.Nested (type (\/))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (maybe)
import Data.Profunctor (lcmap)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (error, log, logShow)
import Effect.Exception (throw)
import Effect.Ref as Ref
import React.Basic (JSX)
import React.Basic.DOM as React
import Snap.Component.SYTC (Cmp)
import Unsafe.Coerce (unsafeCoerce)
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

type Step s i v e = v /\ (i -> Transition s e)

-- }}}

-- {{{ Machines

-- A coalgebra (wow, such math)
type Machine s i v e = s -> Step s i v e

-- Greatest fixpoint of coalgebra
newtype Behavior i v e = Behavior (Step (Behavior i v e) i v e)

newtype EBehavior i v e = EBehavior (Step (Effect (EBehavior i v e)) i v e)

-- Effectful machines
type EMachine s i = Machine s i (ECont i) (ECont i)

type MMachine m s i = Machine s i (m i) (m i)

mapI :: ∀ s i i' v e. (i' -> i) -> Machine s i v e -> Machine s i' v e
mapI = map <<< map <<< lcmap

mapV :: ∀ s i v v' e. (v -> v') -> Machine s i v e -> Machine s i v' e
mapV = map <<< lmap

mapE :: ∀ s i v e e'. (e -> e') -> Machine s i v e -> Machine s i v e'
mapE = map <<< map <<< map <<< map

reportState :: ∀ s i v e.
  Machine s i v e -> Machine s i (s /\ v) e
reportState m s = mapV (s /\ _) m s

-- Unfold
unfoldMachine :: ∀ s i v e.
  Machine s i v e -> s -> Effect (EBehavior i v e)
unfoldMachine machine init = do
  ref <- Ref.new init
  go ref
  where
  go ref = do
    s <- Ref.read ref
    pure $ EBehavior $ case machine s of
      v /\ f -> v /\ f >>> case _ of
        No -> No
        Yes s' e -> flip Yes e $ do
          Ref.write s' ref
          go ref

-- {{{ Pure encapsulate

-- TODO: Make these names not suck

-- addVDoms :: ∀ m v s u.
--   Cmp m v s u ->
--   Machine s u (K (m Unit) (     m Unit) u) (MCont m u) ->
--   Machine s u (K (m Unit) (v /\ m Unit) u) (MCont m u)
-- addVDoms c = reportState >>> mapV (lmap (flip c) >>> K.parallel >>> K.mapI K.dup >>> K.map K.merge)
-- 
-- makeABullshit :: ∀ m v s u.
--        Machine s u (K (m Unit) (v /\ m Unit) u) (MCont m u) ->
--   s -> Behavior  u (K (m Unit) (v /\ m Unit) u) (MCont m u)
-- makeABullshit = unfoldMachine
-- 
-- something :: ∀ m v u. MonadEffect m =>
--   Behavior u (K (m Unit) (v /\ m Unit) u) (MCont m u) ->
--   K (m Unit) (m (u -> Transition (Behavior u (K (m Unit) (v /\ m Unit) u) (MCont m u)) (MCont m u))) v
-- something b cb = go b
--   where
--   handleUpdate t u = case t u of
--     No -> liftEffect $ throw "Bad transition applied, to you sir we say: why don't you go and fuck off then"
--     Yes rest effect -> do
--       t' <- pure unit >>= \_ -> go rest
--       effect (handleUpdate t')
-- 
--   go (Behavior (vertex /\ transition)) = do
--     let vdom /\ background = vertex (handleUpdate transition)
--     -- Render the UI
--     cb vdom
--     -- Run the background task
--     background
--     -- Return the transition
--     pure transition
-- 
-- theOverallThing :: ∀ m v s u. MonadEffect m =>
--   Cmp m v s u ->
--   Machine s u (MCont m u) (MCont m u) ->
--   s -> MCont m v
-- theOverallThing c m = K.mapO (map mempty) <<< something <<< makeABullshit (addVDoms c m)

-- }}}

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

par :: ∀ v1 v2.
  ECont v1 /\ ECont v2 -> ECont (v1 \/ v2)
par (k1 /\ k2) cb = k1 (cb <<< Left) *> k2 (cb <<< Right)

esplice :: forall s1 s2 i1 i2.
  EMachine s1 i1 -> EMachine s2 i2 -> EMachine (s1 /\ s2) (i1 \/ i2)
esplice m1 m2 = mapE K.diverge $ mapV par $ splice m1 m2

-- }}}

-- encapsulate :: ∀ s u v.
--   EMachine s u -> Cmp Effect v s u -> s -> ECont v
-- encapsulate m render s = \cb -> do
--   let
--     handleNewState current = do
--       -- Get the background task for the current state
--       let background /\ _ = m current
--       -- Render the new UI
--       cb $ render (handleUpdate current) current
--       -- Run the background task
--       -- Order is important here, the background task must be run after the component is rendered
--       background (handleUpdate current)
-- 
--     handleUpdate s' u = do
--       -- Run the machine to get the transition
--       let _ /\ transition = m s'
--       -- Run the transition to get the next state and the side effect
--       case transition u of
--         No -> error "Bad transition was applied to the machine, to you sir we say: 🖕"
--         Yes nextState effect -> do
--           -- Render the new UI
--           handleNewState nextState
--           -- Run the side effect
--           effect (handleUpdate nextState)
-- 
--   handleNewState s

-- }}}


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

  Idle -> {- Note: We're just ignoring vertex effects because they suck and Asad sucks ass -} K.empty /\ case _ of
    Load -> Yes Loading $ (fetch K.<#> either Fail Succeed) 
    _ -> No

  Loading -> K.empty /\ case _ of
    Succeed url -> Yes (Success url) K.empty
    Fail error -> Yes (Failure error) K.empty
    _ -> No

  Success url -> K.empty /\ case _ of
    Load -> Yes Idle K.empty
    _ -> No

  Failure error -> K.empty /\ case _ of
    Load -> Yes Idle K.empty
    _ -> No

-- }}}

runReact :: ∀ m. MonadEffect m => Element -> MCont m JSX -> m Unit
runReact e f = f \v -> liftEffect $ React.render v e

element :: String -> Effect Element
element id = do
  mc <- window >>= document <#> toNonElementParentNode >>= getElementById id
  maybe (throw "Couldn't find root element") pure mc

simpleMain :: ∀ s u. String -> EMachine s u -> Cmp Effect JSX s u -> s -> Effect Unit
simpleMain id machine cmp s = do
  elem <- element id
  pure unit
  -- runReact elem $ theOverallThing cmp machine s

scratch :: Effect Unit
scratch = do
  -- Produce the machine, unfold it, and run it on some initial state
  let
    fm d msg = fetchMachine $ \cb -> launchAff_ (delay (Milliseconds d) *> liftEffect (cb $ (Right msg :: Either Void String)))
    m = reportState $ fm 5000.0 "machine 1" `esplice` fm 1000.0 "machine 2"
  ref <- Ref.new $ Idle /\ Idle
  handleUpdate m ref $ Left Load
  handleUpdate m ref $ Right Load

  where
  handleUpdate m ref u = do
    log $ "Transition occurred: " <> show u
    s <- Ref.read ref

    -- Display the present state
    log $ "Current state: " <> show s

    let _ /\ transition = m s
    case transition u of
      No -> log "Produced NO"
      Yes s' effect -> do
        -- Do a prompt to stop
        w <- window
        _ <- prompt "Valid transition occurred. Press OK to continue" w

        -- Write the new state
        log $ "Writing new state" <> show s'
        Ref.write s' ref

        -- Do the transition effect
        log $ "Running transition effects for the old state"
        effect $ handleUpdate m ref
